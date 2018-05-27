FILE  5 = CARD, UNIT = READER, RECORD = 10, BUFFER = 2                  00001000
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00001100
FILE  3 = TAPE3,UNIT=TAPE,RECORD=50,BUFFER=2                            00001200
FILE  2 = TAPE2,UNIT=TAPE,RECORD=50,BUFFER=2                            00001300
CBMD03R  MULTIPLE REGRESSION WITH CASE COMBINATIONS  AUGUST 13, 1964    00001400
      DIMENSION DATA(50),SR(50,50),SD(50,50),ND1(28),AA(50,50),B(50),   00001500
     1SB(50),T(50),ST(50),AV(120),ND(28),P(50,50),DSR(50,50),DSD(50,50),00001600
     2NV(50),NNEWA(50),LLCODE(50),LLVA(50),RES(50),CV(30)               00001700
      COMMON DATA,SR,SD,ND1,AA,B,SB,T,ST,AV,ND,P,DSR,DSD,NV,NNEWA,LLCODE00001800
     1,LLVA,RES,CV,BO,IND,NOOB,NDP,NDEL,OBNO,COUNT,JOB,NOPR,LN,CODET,COR00001900
     2,SSAR,SSDR,VARI,SEE,N,SS1,F,LLM,SS2,LLN,NSEL,NVG,ISAMP,LCASE,MERRY00002000
     3,NADD,NGROUP,NTNOOB,KVR,NTAPE,NT2,NT3                             00002100
C                                                                       00002200
 5010 FORMAT(53H1BMD03R - MULTIPLE REGRESSION WITH CASE COMBINATIONS-   00002300
     X       26HVERSION OF AUGUST 13, 1964/                             00002400
     X  41H HEALTH SCIENCES COMPUTING FACILITY, UCLA//)                 00002500
C                                                                       00002600
      NTAPE=5                                                           00002700
      NT2=2                                                             00002800
      NT3=3                                                             00002900
      REWIND NT2                                                        00003000
      REWIND NT3                                                        00003100
      RTEST=0.0                                                         00003200
      READ           (5,9)BO,NGROUP,IND,NTNOOB,MTAPE,KVR                00003300
      SEE=(+6HPROBLM)                                                   00003400
      IF (SEE.EQ.BO) GO TO 14                                           00003500
   13 WRITE            (6, 1013)                                        00003600
      GO TO 352                                                         00003700
   14 IIND=IND                                                          00003800
      IF((IND-1)*(IND-51)) 15,13,13                                     00003900
 15   IF(NTNOOB-99999)17,17,13                                          00004000
   17 IF(NGROUP*(NGROUP-29)) 18,13,13                                   00004100
 18   IF((MTAPE-NT2)*(MTAPE-NT3))20,13,20                               00004200
 20   CALL TPWD(MTAPE,NTAPE)                                            00004300
      CALL DATAN                                                        00004400
      READ           (5, 10) BO,JOB,NRE,NVG,NADD,NSEL,(ND(I),I=1,28)    00004500
      SEE=(+6HSELECT)                                                   00004600
      IF (SEE.EQ.BO) GO TO 16                                           00004700
 3900 WRITE            (6, 1014)                                        00004800
      GO TO 3915                                                        00004900
 16   JOBD=JOB                                                          00005000
      IF(NSEL*(NSEL-29)) 19,3900,3900                                   00005100
   19 IND=IIND                                                          00005200
      NOPR=0                                                            00005300
      NDEL=0                                                            00005400
      ISAMP=0                                                           00005500
      MERRY=0                                                           00005600
      LCASE=0                                                           00005700
      WRITE            (6, 5010)                                        00005800
      WRITE(6,499) JOB,NOPR                                             00005900
      WRITE            (6, 1008) (ND(I), I=1,NSEL)                      00006000
      WRITE            (6, 510)                                         00006100
      IF(NVG)4000,4000,4001                                             00006200
 4001 IF(NVG-50)400,400,3900                                            00006300
 400  WRITE            (6,5004)                                         00006400
      WRITE            (6,5005)                                         00006500
      SEE=(+6HTRNGEN)                                                   00006600
      DO 4002 I=1,NVG                                                   00006700
      READ           (5, 5006) BO,NNEWA(I),LLCODE(I),LLVA(I),RES(I)     00006800
      IF (SEE.NE.BO) GO TO 3910                                         00006900
 405  IF(LLCODE(I)*(17-LLCODE(I)))3910,3910,4002                        00007000
 3910 WRITE            (6, 1015) I                                      00007100
      IND=-IND                                                          00007200
 4002 WRITE            (6,5007)I,NNEWA(I),LLCODE(I),LLVA(I),RES(I)      00007300
      IF(IND)352,4000,4000                                              00007400
 4000 CALL READ                                                         00007500
      IF(IND)3900,3900,410                                              00007600
 410  IF(NOOB-ISAMP)4003,4003,4004                                      00007700
 4004 OBNO=ISAMP                                                        00007800
      NOOB=OBNO                                                         00007900
 4003 IND=IND+NADD                                                      00008000
      NDP=IND                                                           00008100
      IF(NOOB-1-IND) 4005,4006,4006                                     00008200
 4005 WRITE            (6,5008)NOOB                                     00008300
      WRITE            (6,5009)                                         00008400
 3915 MERRY=-999                                                        00008500
      GO TO 4007                                                        00008600
 4006 WRITE            (6, 91)                                          00008700
      WRITE            (6, 92) (SB(I), I=1,IND)                         00008800
      WRITE            (6, 510)                                         00008900
      WRITE            (6, 93)                                          00009000
      WRITE            (6, 92) (T(I), I=1,IND)                          00009100
      WRITE            (6, 510)                                         00009200
      WRITE            (6,1000)                                         00009300
      DO 2000 I=1,IND                                                   00009400
      WRITE            (6, 173) I                                       00009500
 2000 WRITE            (6,92)(DSR(I,J), J=1,IND)                        00009600
      DO 112 I=1,IND                                                    00009700
      AV(I)=SB(I)/OBNO                                                  00009800
      DATA(I)=T(I)-((SB(I)*SB(I))/OBNO)                                 00009900
  112 ST(I)=SQRT (DATA(I)/(OBNO-1.0))                                   00010000
      DO171I=1,IND                                                      00010100
      DO 171 J=1,IND                                                    00010200
      DSD(I,J)=DSR(I,J)-((SB(I)*SB(J))/OBNO)                            00010300
      DSD(I,J)=DSD(I,J)+1.E-25                                          00010400
  171 DSR(I,J)=DSD(I,J)/SQRT (DATA(I)*DATA(J))                          00010500
      WRITE            (6, 510)                                         00010600
      WRITE            (6, 1011)                                        00010700
      DO 175 I=1,IND                                                    00010800
      WRITE            (6, 173) I                                       00010900
  175 WRITE            (6, 92) (DSD(I,J), J=1,IND)                      00011000
   56 LN=IND                                                            00011100
      MM=LN-1                                                           00011200
      DO 54 I=1,LN                                                      00011300
   54 NV(I)=I                                                           00011400
      IF(NDEL) 2987, 2987, 1018                                         00011500
 1018 DO 2985 I=1,NDEL                                                  00011600
      KK=ND(I)                                                          00011700
 2985 NV(KK)=0                                                          00011800
      GO TO 2990                                                        00011900
 2987 ND(1)=0                                                           00012000
 2990 IF(NDP-LN) 2993, 3000, 2992                                       00012100
 2992 WRITE            (6, 1016)                                        00012200
      GO TO 4007                                                        00012300
 2993 DO 2994 I=NDP,MM                                                  00012400
 2994 NV(I)=NV(I+1)                                                     00012500
      NV(LN)=NDP                                                        00012600
 3000 IF(NDEL) 3050, 3050, 3030                                         00012700
 3030 LN=0                                                              00012800
      DO 3045 I=1,IND                                                   00012900
      IF(NV(I)) 3045, 3045, 3042                                        00013000
 3042 LN=LN+1                                                           00013100
      NV(LN)=NV(I)                                                      00013200
 3045 CONTINUE                                                          00013300
 3050 DO 3060 J=1,LN                                                    00013400
      MM=NV(J)                                                          00013500
      DO 3060 I=1,LN                                                    00013600
      KK=NV(I)                                                          00013700
      SR(I,J)=DSR(KK,MM)                                                00013800
 3060 SD(I,J)=DSD(KK,MM)                                                00013900
   60 WRITE            (6, 500) JOB,NOPR                                00014000
      WRITE            (6, 172)                                         00014100
      DO 181 I=1,LN                                                     00014200
      WRITE            (6, 173)I                                        00014300
  181 WRITE            (6, 174) (SR(I,J), J=1,LN)                       00014400
      N=LN-1                                                            00014500
      DO 1060 J=1,LN                                                    00014600
      DO 1060 I=1,LN                                                    00014700
 1060 P(I,J)=SR(I,J)                                                    00014800
      CALL INVERT (P,LN,DET,NNEWA,LLCODE)                               00014900
      DO 1063 I=1,N                                                     00015000
 1063 RES(I)=-P(I,LN)/SQRT (P(LN,LN)*P(I,I))                            00015100
      IF(N-1) 65, 65, 67                                                00015200
   65 AA(1,1)=1.0                                                       00015300
      GO TO 239                                                         00015400
   67 DO 70 I=1,N                                                       00015500
      DO 70 J=1,N                                                       00015600
   70 AA(I,J)=SR(I,J)                                                   00015700
      CALL INVERT(AA,N,DET,NNEWA,LLCODE)                                00015800
      WRITE            (6, 510)                                         00015900
      WRITE            (6,1009)                                         00016000
      DO 231 I=1,N                                                      00016100
      WRITE            (6,173)I                                         00016200
  231 WRITE            (6,174)(AA(I,J),J=1,N)                           00016300
  239 DO 240 I=1,N                                                      00016400
      DO 240 J=1,N                                                      00016500
  240 SR(I,J)=AA(I,J)*SR(I,J)/SD(I,J)                                   00016600
      DO 280 J=1,N                                                      00016700
      SUM=0.0                                                           00016800
      DO 270 I=1,N                                                      00016900
  270 SUM=SUM+SR(I,J)*SD(I,LN)                                          00017000
  280 B(J)=SUM                                                          00017100
      SSAR=0.0                                                          00017200
      DO 290 I=1,N                                                      00017300
  290 SSAR=SSAR+B(I)*SD(I,LN)                                           00017400
      SSDR=SD(LN,LN)-SSAR                                               00017500
      CODET=SSAR/SD(LN,LN)                                              00017600
      COR=SQRT (CODET)                                                  00017700
      DF=LN                                                             00017800
      VARI=SSDR/(OBNO-DF)                                               00017900
      SEE=SQRT (VARI)                                                   00018000
      DO 300 J=1,N                                                      00018100
      SUM=VARI*SR(J,J)                                                  00018200
  300 SB(J)=SQRT (SUM)                                                  00018300
      SUM=0.0                                                           00018400
      DO 310 I=1,N                                                      00018500
      K=NV(I)                                                           00018600
  310 SUM=SUM+B(I)*AV(K)                                                00018700
      K=NV(LN)                                                          00018800
      BO=AV(K)-SUM                                                      00018900
      DO 320 I=1,N                                                      00019000
  320 T(I)=B(I)/SB(I)                                                   00019100
      D1=N                                                              00019200
      D2=OBNO-DF                                                        00019300
      D3=OBNO-1.0                                                       00019400
      LLM=D2                                                            00019500
      LLN=D3                                                            00019600
      SS1=SSAR/D1                                                       00019700
      SS2=SSDR/D2                                                       00019800
      F=SS1/SS2                                                         00019900
      NOOB=OBNO                                                         00020000
      CALL REPORT                                                       00020100
      IF(NRE) 4007, 4007, 348                                           00020200
  348 CALL RESIDU (NRE,RTEST)                                           00020300
      RTEST=RTEST+1.0                                                   00020400
 4007 READ           (5, 10) BO,JOB,NOPR,NRE,NDP,NDEL,(ND(I),I=1,28)    00020500
      IF(JOB-JOBD)351,4008,351                                          00020600
 4008 SEE=(+6HREPDEL)                                                   00020700
      IF (SEE.NE.BO) GO TO 351                                          00020800
 4009 IF(MERRY)  4007,56,56                                             00020900
  351 MERRY=0                                                           00021000
      SEE=(+6HREPDEL)                                                   00021100
      IF (SEE.EQ.BO) GO TO 117                                          00021200
  114 SEE=(+6HSELECT)                                                   00021300
      IF (SEE.EQ.BO) GO TO 115                                          00021400
  116 SEE=(+6HFINISH)                                                   00021500
      IF (SEE.EQ.BO) GO TO 352                                          00021600
      GO TO 119                                                         00021700
  117 WRITE            (6,1001)                                         00021800
  118 WRITE            (6,1002)                                         00021900
      MERRY=-999                                                        00022000
      GO TO 4007                                                        00022100
  119 WRITE            (6,1003)BO                                       00022200
      GO TO 118                                                         00022300
  115 NVG=NRE                                                           00022400
      NRE=NOPR                                                          00022500
      NADD=NDP                                                          00022600
      NSEL=NDEL                                                         00022700
      CALL TPWD(MTAPE,NTAPE)                                            00022800
      GO TO 16                                                          00022900
C                                                                       00023000
 352  IF(NTAPE-5)354,354,353                                            00023100
C353  CALL REMOVE(NTAPE)                                                00023200
 353  CONTINUE                                                          00023300
      WRITE            (6, 499) JOB,NOPR                                00023400
 354  CALL EXIT                                                         00023500
C                                                                       00023600
    9 FORMAT(A6,2I2,I5,I2,53X,I2)                                       00023700
   10 FORMAT(A6,33I2)                                                   00023800
   91 FORMAT(5H0SUMS)                                                   00023900
   92 FORMAT (1H F19.5,5F20.5)                                          00024000
   93 FORMAT(15H0SUM OF SQUARES)                                        00024100
  172 FORMAT(25H0CORRELATION COEFFICIENTS)                              00024200
  173 FORMAT(6H0ROW  I2)                                                00024300
  174 FORMAT (1H F9.5,11F10.5)                                          00024400
  499 FORMAT(14H0SELECTION NO.I5,1H-I2)                                 00024500
  500 FORMAT(14H1SELECTION NO.I5,1H-I2//)                               00024600
  510 FORMAT(1H0)                                                       00024700
 1000 FORMAT (19H0CROSS PRODUCT SUMS)                                   00024800
 1001 FORMAT(91H0ERRONEOUS REPDEL CARD ORDER. SELECTION NUMBER DOES NOT 00024900
     XAGREE WITH PREVIOUS SELECTION CARD.)                              00025000
 1002 FORMAT(46H PROGRAM SKIPS TO NEXT SELECTION CARD, IF ANY.)         00025100
 1003 FORMAT(73H0CONTROL CARD ORDER ERROR. PROGRAM EXPECTS A SELECT OR R00025200
     XEPDEL CARD BUT A ,A6,18H CARD WAS PRESENT.)                       00025300
 1008 FORMAT(21H SUB-SAMPLES SELECTED28I3)                              00025400
 1009 FORMAT(42H0INVERSE OF CORRELATION COEFFICIENT MATRIX)             00025500
 1011 FORMAT(29H0CROSS PRODUCTS OF DEVIATIONS)                          00025600
 1013 FORMAT(22H0ERROR ON PROBLEM CARD)                                 00025700
 1014 FORMAT(24H0ERROR ON SELECTION CARD)                               00025800
 1015 FORMAT(31H0ERROR ON TRANS-GENERATION CARDI4)                      00025900
 1016 FORMAT(35H0ERROR ON REPLACEMENT-DELETION CARD)                    00026000
 5004 FORMAT(1H06X,23HTRANS GENERATOR CARD(S))                          00026100
 5005 FORMAT(46H0CARD    NEW     TRANS    ORIG.   ORIG. VAR(B)/45H  NO. 00026200
     1VARIABLE   CODE    VAR(A)   OR CONSTANT)                          00026300
 5006 FORMAT(A6,I3,I2,I3,F6.0)                                          00026400
 5007 FORMAT(2H  I2,I8,2I9,F14.4)                                       00026500
 5008 FORMAT(22H0SAMPLE SIZE RETAINED=I7)                               00026600
 5009 FORMAT(67H0INSUFFICIENT SAMPLE,PROGRAM WILL GO TO NEXT SELECTION,E00026700
     1TC., IF ANY)                                                      00026800
C                                                                       00026900
      END                                                               00027000
                                                                        00027100
      SUBROUTINE DATAN                                                  00027200
CDATA         SUBROUTINE DATAN FOR BMD03R           AUGUST 13, 1964     00027300
      DIMENSION DATA(50),SR(50,50),SD(50,50),ND1(28),AA(50,50),B(50),   00027400
     1SB(50),T(50),ST(50),AV(120),ND(28),P(50,50),DSR(50,50),DSD(50,50),00027500
     2NV(50),NNEWA(50),LLCODE(50),LLVA(50),RES(50),CV(30)               00027600
      COMMON DATA,SR,SD,ND1,AA,B,SB,T,ST,AV,ND,P,DSR,DSD,NV,NNEWA,LLCODE00027700
     1,LLVA,RES,CV,BO,IND,NOOB,NDP,NDEL,OBNO,COUNT,JOB,NOPR,LN,CODET,COR00027800
     2,SSAR,SSDR,VARI,SEE,N,SS1,F,LLM,SS2,LLN,NSEL,NVG,ISAMP,LCASE,MERRY00027900
     3,NADD,NGROUP,NTNOOB,KVR,NTAPE,NT2,NT3                             00028000
      TOTAL=(+6HTOTAL )                                                 00028100
      SUM=(+6H SUM  )                                                   00028200
      READ           (5, 900) BO,(ND1(I), I=1,NGROUP)                   00028300
      SEE=(+6HSAMSIZ)                                                   00028400
      IF (SEE.EQ.BO) GO TO 15                                           00028500
   10 WRITE            (6, 902)                                         00028600
      CALL EXIT                                                         00028700
   15 CALL VFCHCK(KVR)                                                  00028800
      KVR=KVR*12                                                        00028900
      NTOTAL=0                                                          00029000
      DO 20 I=1,NGROUP                                                  00029100
   20 NTOTAL=NTOTAL+ND1(I)                                              00029200
      SIZE=TOTAL                                                        00029300
      IF(NTNOOB-NTOTAL)24,45,25                                         00029400
   24 SIZE=SUM                                                          00029500
      NTNOOB=NTOTAL                                                     00029600
   25 WRITE            (6,903)SIZE                                      00029700
   45 READ           (5, 901)(AV(I),I=1,KVR)                            00029800
      DO 55 J=1,NTNOOB                                                  00029900
   49 READ           (NTAPE, AV) (DATA(I), I=1,IND)                     00030000
   55 WRITE     (NT3)(DATA(I),I=1,IND)                                  00030100
   60 END FILE NT3                                                      00030200
      REWIND NT3                                                        00030300
      RETURN                                                            00030400
  900 FORMAT(A6,11I6/(6X,11I6))                                         00030500
  901 FORMAT(12A6)                                                      00030600
  902 FORMAT(26H0ERROR ON SAMPLE SIZE CARD)                             00030700
  903 FORMAT(81H0TOTAL SAMPLE SIZE AND SUM OF SAMPLE SIZES DO NOT AGREE.00030800
     X THE PROGRAM ASSUMES THE A6,28H TO BE CORRECT AND PROCEEDS.)      00030900
      END                                                               00031000
                                                                        00031100
      SUBROUTINE INVERT (A,N,D,L,M)                                     00031200
C     PROGRAM FOR FINDING THE INVERSE OF A NXN MATRIX                   00031300
      DIMENSION A(50,50),L(50),M(50)                                    00031400
C     SEARCH FOR LARGEST ELEMENT                                        00031500
      D=1.0                                                             00031600
      DO80 K=1,N                                                        00031700
      L(K)=K                                                            00031800
      M(K)=K                                                            00031900
      BIGA=A(K,K)                                                       00032000
      DO20 I=K,N                                                        00032100
      DO20 J=K,N                                                        00032200
      IF(ABS (BIGA)-ABS (A(I,J))) 10,20,20                              00032300
   10 BIGA=A(I,J)                                                       00032400
      L(K)=I                                                            00032500
      M(K)=J                                                            00032600
   20 CONTINUE                                                          00032700
C     INTERCHANGE ROWS                                                  00032800
      J=L(K)                                                            00032900
      IF(L(K)-K) 35,35,25                                               00033000
   25 DO30 I=1,N                                                        00033100
      HOLD=-A(K,I)                                                      00033200
      A(K,I)=A(J,I)                                                     00033300
   30 A(J,I)=HOLD                                                       00033400
C     INTERCHANGE COLUMNS                                               00033500
   35 I=M(K)                                                            00033600
      IF(M(K)-K) 45,45,37                                               00033700
   37 DO40 J=1,N                                                        00033800
      HOLD=-A(J,K)                                                      00033900
      A(J,K)=A(J,I)                                                     00034000
   40 A(J,I)=HOLD                                                       00034100
C     DIVIDE COLUMN BY MINUS PIVOT                                      00034200
   45 DO55 I=1,N                                                        00034300
   46 IF(I-K)50,55,50                                                   00034400
   50 A(I,K)=A(I,K)/(-A(K,K))                                           00034500
   55 CONTINUE                                                          00034600
C     REDUCE MATRIX                                                     00034700
      DO65 I=1,N                                                        00034800
      DO65 J=1,N                                                        00034900
   56 IF(I-K) 57,65,57                                                  00035000
   57 IF(J-K) 60,65,60                                                  00035100
   60 A(I,J)=A(I,K)*A(K,J)+A(I,J)                                       00035200
   65 CONTINUE                                                          00035300
C     DIVIDE ROW BY PIVOT                                               00035400
      DO75 J=1,N                                                        00035500
   68 IF(J-K)70,75,70                                                   00035600
   70 A(K,J)=A(K,J)/A(K,K)                                              00035700
   75 CONTINUE                                                          00035800
C     CONTINUED PRODUCT OF PIVOTS                                       00035900
      D=D*A(K,K)                                                        00036000
C     REPLACE PIVOT BY RECIPROCAL                                       00036100
      A(K,K)=1.0/A(K,K)                                                 00036200
   80 CONTINUE                                                          00036300
C     FINAL ROW AND COLUMN INTERCHANGE                                  00036400
      K=N                                                               00036500
  100 K=(K-1)                                                           00036600
      IF(K) 150,150,103                                                 00036700
  103 I=L(K)                                                            00036800
      IF(I-K) 120,120,105                                               00036900
  105 DO110 J=1,N                                                       00037000
      HOLD=A(J,K)                                                       00037100
      A(J,K)=-A(J,I)                                                    00037200
  110 A(J,I)=HOLD                                                       00037300
  120 J=M(K)                                                            00037400
      IF(J-K) 100,100,125                                               00037500
  125 DO130 I=1,N                                                       00037600
      HOLD=A(K,I)                                                       00037700
      A(K,I)=-A(J,I)                                                    00037800
  130 A(J,I)=HOLD                                                       00037900
      GO TO 100                                                         00038000
  150 RETURN                                                            00038100
      END                                                               00038200
                                                                        00038300
      SUBROUTINE READ                                                   00038400
CREAD    SUBROUTINE READ FOR BMD03R                    MAY 27, 1964     00038500
      DIMENSION DATA(50),SR(50,50),SD(50,50),ND1(28),AA(50,50),B(50),   00038600
     1SB(50),T(50),ST(50),AV(120),ND(28),P(50,50),DSR(50,50),DSD(50,50),00038700
     2NV(50),NNEWA(50),LLCODE(50),LLVA(50),RES(50),CV(30)               00038800
C                                                                       00038900
      COMMON DATA,SR,SD,ND1,AA,B,SB,T,ST,AV,ND,P,DSR,DSD,NV,NNEWA,LLCODE00039000
     1,LLVA,RES,CV,BO,IND,NOOB,NDP,NDEL,OBNO,COUNT,JOB,NOPR,LN,CODET,COR00039100
     2,SSAR,SSDR,VARI,SEE,N,SS1,F,LLM,SS2,LLN,NSEL,NVG,ISAMP,LCASE,MERRY00039200
     3,NADD,NGROUP,NTNOOB,KVR,NTAPE,NT2,NT3                             00039300
C                                                                       00039400
      LLM=IND+NADD                                                      00039500
      IF((LLM-1)*(LLM-51)) 1,100,100                                    00039600
 1    DO 20 I=1,50                                                      00039700
      SB(I)=0.0                                                         00039800
      T(I)=0.0                                                          00039900
      DO 20 J=1,50                                                      00040000
   20 DSR(I,J)=0.0                                                      00040100
      DO 21 I=1,NGROUP                                                  00040200
   21 NV(I)=-ND1(I)                                                     00040300
      DO 25 I=1,NSEL                                                    00040400
      NQ=ND(I)                                                          00040500
      IF(NQ)25,25,22                                                    00040600
 22   NV(NQ)=NV(NQ)*(-1)                                                00040700
 25   CONTINUE                                                          00040800
      NTAB=0                                                            00040900
      NOOB=0                                                            00041000
      DO 6001 I8=1,NGROUP                                               00041100
      I9=NV(I8)                                                         00041200
      IF(I9) 5871, 6001, 5901                                           00041300
 5871 I9=I9*(-1)                                                        00041400
      DO 5875 M9=1,I9                                                   00041500
 5875 READ     (NT3)(DATA(I),I=1,IND)                                   00041600
      GO TO 6001                                                        00041700
 5901 DO 6000 M9=1,I9                                                   00041800
      READ     (NT3)(DATA(I),I=1,IND)                                   00041900
      IF(NVG) 66, 66, 201                                               00042000
  201 CALL TRNGEN(DATA,IND,NVG,I9,ISAMP,LCASE,NNEWA,LLCODE,LLVA,RES,    00042100
     1MERRY,M9)                                                         00042200
      IF(LCASE) 203, 66, 66                                             00042300
  203 LCASE=0                                                           00042400
      GO TO 6000                                                        00042500
   66 DO 67 I=1,LLM                                                     00042600
      SB(I)=SB(I)+DATA(I)                                               00042700
   67 T(I)=T(I)+DATA(I)*DATA(I)                                         00042800
      DO 151 I=1,LLM                                                    00042900
      DO 151 J=1,LLM                                                    00043000
  151 DSR(I,J)=DSR(I,J)+DATA(I)*DATA(J)                                 00043100
      WRITE     (NT2)(DATA(I),I=1,LLM)                                  00043200
 6000 CONTINUE                                                          00043300
      NOOB=NOOB+I9                                                      00043400
      NTAB=NTAB+1                                                       00043500
      IF(NSEL-NTAB) 81, 81, 6001                                        00043600
 6001 CONTINUE                                                          00043700
   81 END FILE NT2                                                      00043800
      REWIND NT2                                                        00043900
      REWIND NT3                                                        00044000
      OBNO=NOOB                                                         00044100
      ISAMP=NOOB-ISAMP                                                  00044200
 110  RETURN                                                            00044300
 100  IND=-LLM                                                          00044400
      GO TO 110                                                         00044500
C                                                                       00044600
      END                                                               00044700
                                                                        00044800
      SUBROUTINE REPORT                                                 00044900
CREPO        SUBROUTINE REPORT FOR BMD03R                         9-5-6300045000
      DIMENSION DATA(50),SR(50,50),SD(50,50),ND1(28),AA(50,50),B(50),   00045100
     1SB(50),T(50),ST(50),AV(120),ND(28),P(50,50),DSR(50,50),DSD(50,50),00045200
     2NV(50),NNEWA(50),LLCODE(50),LLVA(50),RES(50),CV(30)               00045300
      COMMON DATA,SR,SD,ND1,AA,B,SB,T,ST,AV,ND,P,DSR,DSD,NV,NNEWA,LLCODE00045400
     1,LLVA,RES,CV,BO,IND,NOOB,NDP,NDEL,OBNO,COUNT,JOB,NOPR,LN,CODET,COR00045500
     2,SSAR,SSDR,VARI,SEE,N,SS1,F,LLM,SS2,LLN,NSEL,NVG,ISAMP,LCASE,MERRY00045600
     3,NADD,NGROUP,NTNOOB,KVR,NTAPE                                     00045700
      WRITE           (6,500) JOB, NOPR,NOOB                            00045800
      WRITE           (6,502)LN,NDEL                                    00045900
      WRITE            (6, 1005) NDP                                    00046000
      WRITE           (6,503)CODET                                      00046100
      WRITE           (6,504)COR                                        00046200
      WRITE           (6,505)SSAR                                       00046300
      WRITE           (6,506)SSDR                                       00046400
      WRITE           (6,507)VARI                                       00046500
      WRITE           (6,508)SEE                                        00046600
      WRITE           (6,509)BO                                         00046700
      WRITE           (6,511)                                           00046800
      WRITE           (6,512)                                           00046900
      WRITE           (6,513)                                           00047000
      WRITE           (6,514)                                           00047100
      WRITE           (6,515)N,SSAR,SS1,F                               00047200
      WRITE           (6,516)LLM,SSDR,SS2                               00047300
      WRITE           (6,517)LLN,SD(LN,LN)                              00047400
      DO 10 J=1,LN                                                      00047500
 10   SR(1,J)=SD(1,J)                                                   00047600
      DO 30 I=1,N                                                       00047700
      II=I                                                              00047800
      DO 20 J=II,LN                                                     00047900
 20   P(I,J)=SR(I,J)/SR(I,I)                                            00048000
      K=I+1                                                             00048100
      DO 30 J=K,LN                                                      00048200
      SUM=0.0                                                           00048300
      DO 25 M=1,II                                                      00048400
 25   SUM=SUM+P(M,K)*SR(M,J)                                            00048500
 30   SR(K,J)=SD(K,J)-SUM                                               00048600
      DO 40 I=1,N                                                       00048700
 40   AA(I,1)=SR(I,LN)*P(I,LN)                                          00048800
      DO 45 I=1,N                                                       00048900
 45   AA(I,2)=AA(I,1)/SD(LN,LN)                                         00049000
      WRITE           (6,518)                                           00049100
      WRITE           (6,519)                                           00049200
      DO350 I=1,N                                                       00049300
      K=NV(I)                                                           00049400
 350  WRITE           (6,520)NV(I),AV(K),ST(K),B(I),SB(I),T(I),RES(I),  00049500
     1AA(I,1),AA(I,2)                                                   00049600
      K=NV(LN)                                                          00049700
      WRITE            (6, 521)NV(LN),AV(K),ST(K)                       00049800
      WRITE            (6, 522) P(N,LN)                                 00049900
      WRITE            (6, 1013) (ND(I), I=1,NDEL)                      00050000
  500 FORMAT(14H1SELECTION NO.I5,1H-I2//12H SAMPLE SIZEI8)              00050100
  502 FORMAT(17H NO. OF VARIABLESI3,30H      NO. OF VARIABLES DELETEDI3,00050200
     136H  (FOR VARIABLES DELETED, SEE BELOW))                          00050300
  503 FORMAT(29H0COEFFICIENT OF DETERMINATIONF8.4)                      00050400
  504 FORMAT(29H MULTIPLE CORR. COEFFICIENT  F8.4)                      00050500
  505 FORMAT(44H0SUM OF SQUARES ATTRIBUTABLE TO REGRESSION  F15.5)      00050600
  506 FORMAT(44H SUM OF SQUARES OF DEVIATION FROM REGRESSIONF15.5)      00050700
  507 FORMAT(23H0VARIANCE OF ESTIMATE  F12.5)                           00050800
  508 FORMAT(23H STD. ERROR OF ESTIMATEF12.5)                           00050900
  509 FORMAT(20H0INTERCEPT (A VALUE)F12.5//)                            00051000
  511 FORMAT(54H0                ANALYSIS OF VARIANCE FOR THE MULTIPLE) 00051100
  512 FORMAT(43H                         LINEAR  REGRESSION)            00051200
  513 FORMAT(71H0      SOURCE OF VARIATION       D.F.      SUM OF       00051300
     1 MEAN         F)                                                  00051400
  514 FORMAT(42X,31HSQUARES       SQUARES     VALUE)                    00051500
  515 FORMAT(30H DUE TO REGRESSION............I5,F15.5,F14.5,F11.4)     00051600
  516 FORMAT(30H DEVIATION ABOUT REGRESSION...I5,F15.5,F14.5)           00051700
  517 FORMAT(22X,8HTOTAL...I5,F15.5//)                                  00051800
  518 FORMAT(105H0VARIABLE     MEAN        STD.        REG.    STD.ERROR00051900
     1    COMPUTED    PARTIAL     SUM OF SQ.  PROP. VAR.)               00052000
  519 FORMAT(101H   NO.                  DEVIATION    COEFF.   OF REG.CO00052100
     1E.  T VALUE    CORR. COE.     ADDED       CUM.)                   00052200
  520 FORMAT(I5,F15.5,F12.5,2F11.5,2F12.5,F15.5,F11.5)                  00052300
  521 FORMAT(I5,F15.5,F12.5)                                            00052400
  522 FORMAT(32H0COMP. CHECK ON FINAL COEFF.    F11.5//)                00052500
 1005 FORMAT(30H DEPENDENT VARIABLE IS NOW NO.I3)                       00052600
 1013 FORMAT(21H VARIABLES DELETED...28I3)                              00052700
      RETURN                                                            00052800
      END                                                               00052900
                                                                        00053000
      SUBROUTINE RESIDU (NRE,RTEST)                                     00053100
CRESI        SUBROUTINE RESIDU FOR BMD03R                         9-5-6300053200
      DIMENSION DATA(50),SR(50,50),SD(50,50),ND1(28),AA(50,50),B(50),   00053300
     1SB(50),T(50),ST(50),AV(120),ND(28),P(50,50),DSR(50,50),DSD(50,50),00053400
     2NV(50),NNEWA(50),LLCODE(50),LLVA(50),RES(50),CV(30)               00053500
      COMMON DATA,SR,SD,ND1,AA,B,SB,T,ST,AV,ND,P,DSR,DSD,NV,NNEWA,LLCODE00053600
     1,LLVA,RES,CV,BO,IND,NOOB,NDP,NDEL,OBNO,COUNT,JOB,NOPR,LN,CODET,COR00053700
     2,SSAR,SSDR,VARI,SEE,N,SS1,F,LLM,SS2,LLN,NSEL,NVG,ISAMP,LCASE,MERRY00053800
     3,NADD,NGROUP,NTNOOB,KVR,NTAPE                                     00053900
      SMALL=10.0**36                                                    00054000
      BIG=-SMALL                                                        00054100
      IF(RTEST) 5,5,6                                                   00054200
 5    CV(3)=.886                                                        00054300
      CV(4)=.679                                                        00054400
      CV(5)=.557                                                        00054500
      CV(6)=.482                                                        00054600
      CV(7)=.434                                                        00054700
      CV(8)=.479                                                        00054800
      CV(9)=.441                                                        00054900
      CV(10)=.409                                                       00055000
      CV(11)=.517                                                       00055100
      CV(12)=.490                                                       00055200
      CV(13)=.467                                                       00055300
      CV(14)=.492                                                       00055400
      CV(15)=.472                                                       00055500
      CV(16)=.454                                                       00055600
      CV(17)=.438                                                       00055700
      CV(18)=.424                                                       00055800
      CV(19)=.412                                                       00055900
      CV(20)=.401                                                       00056000
      CV(21)=.391                                                       00056100
      CV(22)=.382                                                       00056200
      CV(23)=.374                                                       00056300
      CV(24)=.367                                                       00056400
      CV(25)=.360                                                       00056500
      CV(26)=.354                                                       00056600
      CV(27)=.348                                                       00056700
      CV(28)=.342                                                       00056800
      CV(29)=.337                                                       00056900
      CV(30)=.332                                                       00057000
 6    IF(NRE-1) 9,9,8                                                   00057100
 8    WRITE            (6,700)                                          00057200
      WRITE            (6,701)                                          00057300
 9    DO 20 K=1,NOOB                                                    00057400
      READ     (2) (DATA(I), I=1,IND)                                   00057500
      YEST=BO                                                           00057600
      DO 10 I=1,N                                                       00057700
      J=NV(I)                                                           00057800
 10   YEST=YEST+B(I)*DATA(J)                                            00057900
      J=NV(LN)                                                          00058000
      RESID=DATA(J)-YEST                                                00058100
      IF(RESID-SMALL) 11, 12, 12                                        00058200
 11   SMALL=RESID                                                       00058300
 12   IF(RESID-BIG) 14, 14, 13                                          00058400
 13   BIG=RESID                                                         00058500
 14   IF(NOOB-30) 15, 15, 16                                            00058600
 15   RES(K)=RESID                                                      00058700
 16   IF(NRE-1) 20,20,19                                                00058800
 19   WRITE            (6,702)K,DATA(J),YEST,RESID                      00058900
 20   CONTINUE                                                          00059000
      REWIND 2                                                          00059100
      WRITE            (6, 703)                                         00059200
      WRITE            (6, 709)                                         00059300
      IF(NOOB-30) 25, 25, 21                                            00059400
 21   W=BIG-SMALL                                                       00059500
      CRIT1=W/SEE                                                       00059600
      WRITE            (6, 704) W                                       00059700
      WRITE            (6, 705) CRIT1                                   00059800
      GO TO 100                                                         00059900
 25   DO 55 I=1,NOOB                                                    00060000
      DO 50 J=I,NOOB                                                    00060100
      IF(RES(I)-RES(J)) 50, 50, 45                                      00060200
 45   BIG=RES(J)                                                        00060300
      RES(J)=RES(I)                                                     00060400
      RES(I)=BIG                                                        00060500
 50   CONTINUE                                                          00060600
 55   CONTINUE                                                          00060700
      IF(NOOB-13) 30,30,26                                              00060800
 26   N2=NOOB-2                                                         00060900
      CRIT1=(RES(3)-RES(1))/(RES(N2)-RES(1))                            00061000
      CRIT2=(RES(NOOB)-RES(N2))/(RES(NOOB)-RES(3))                      00061100
      WRITE            (6, 706) CRIT1                                   00061200
      WRITE            (6, 707) CRIT2                                   00061300
      WRITE            (6, 708) CV(NOOB)                                00061400
      GO TO 100                                                         00061500
 30   IF(NOOB-10) 35, 35, 31                                            00061600
 31   N1=NOOB-1                                                         00061700
      N2=NOOB-2                                                         00061800
      CRIT1=(RES(3)-RES(1))/(RES(N1)-RES(1))                            00061900
      CRIT2=(RES(NOOB)-RES(N2))/(RES(NOOB)-RES(2))                      00062000
      WRITE            (6, 706) CRIT1                                   00062100
      WRITE            (6, 707) CRIT2                                   00062200
      WRITE            (6, 708) CV(NOOB)                                00062300
      GO TO 100                                                         00062400
 35   IF(NOOB-7) 40, 40, 36                                             00062500
 36   N1=NOOB-1                                                         00062600
      CRIT1=(RES(2)-RES(1))/(RES(N1)-RES(1))                            00062700
      CRIT2=(RES(NOOB)-RES(N1))/(RES(NOOB)-RES(2))                      00062800
      WRITE            (6, 706) CRIT1                                   00062900
      WRITE            (6, 707) CRIT2                                   00063000
      WRITE            (6, 708) CV(NOOB)                                00063100
      GO TO 100                                                         00063200
 40   IF(NOOB-2) 100, 100, 41                                           00063300
 41   N1=NOOB-1                                                         00063400
      CRIT1=(RES(2)-RES(1))/(RES(NOOB)-RES(1))                          00063500
      CRIT2=(RES(NOOB)-RES(N1))/(RES(NOOB)-RES(1))                      00063600
      WRITE            (6, 706) CRIT1                                   00063700
      WRITE            (6, 707) CRIT2                                   00063800
      WRITE            (6, 708) CV(NOOB)                                00063900
  700 FORMAT(1H1,19X,18HTABLE OF RESIDUALS)                             00064000
 701  FORMAT(59H0OBSERVATION     Y VALUE         Y ESTIMATE        RESID00064100
     1UAL)                                                              00064200
 702  FORMAT(I7,2F18.5,F16.5)                                           00064300
 703  FORMAT(1H0)                                                       00064400
 704  FORMAT(35H0RANGE OF RESIDUALS................F16.3)               00064500
 705  FORMAT(35H RANGE / STD. ERROR OF ESTIMATE....F16.3)               00064600
 706  FORMAT(47H0RATIO OF RANGES FOR THE SMALLEST RESIDUAL.....F10.3)   00064700
 707  FORMAT(47H RATIO OF RANGES FOR THE LARGEST RESIDUAL......F10.3)   00064800
 708  FORMAT(47H0CRITICAL VALUE OF THE RATIO AT ALPHA = .10 ...F10.3)   00064900
 709  FORMAT(26H0TEST OF EXTREME RESIDUALS)                             00065000
 100  RETURN                                                            00065100
      END                                                               00065200
                                                                        00065300
      SUBROUTINE TPWD(NT1,NT2)                                          00065400
CTPWD    SUBROUTINE TPWD FOR BMD03R         VERSION OF SEPT. 26, 1963   00065500
      IF(NT1)40,10,12                                                   00065600
 10   NT1=5                                                             00065700
 12   IF(NT1-NT2)14,19,14                                               00065800
 14   IF(NT2-5)15,19,17                                                 00065900
   15 REWIND NT2                                                        00066000
      GO TO 19                                                          00066100
C  17 CALL REMOVE(NT2)                                                  00066200
   17  CONTINUE                                                         00066300
   19 IF(NT1-5)18,24,18                                                 00066400
 18   IF(NT1-6)22,40,22                                                 00066500
 22   REWIND NT1                                                        00066600
 24   NT2=NT1                                                           00066700
 28   RETURN                                                            00066800
 40   WRITE            (6,49)                                           00066900
      CALL EXIT                                                         00067000
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              00067100
      END                                                               00067200
                                                                        00067300
      SUBROUTINE TRNGEN(DATA,IND,NVG,NODATA,ISAMP,LCASE,NNEWA,LLCODE,   00067400
CTRNG         SUBROUTINE TRNGEN FOR BMD03R        JANUARY 10, 1964      00067500
     1LLVA,RES,MERRY,N)                                                 00067600
      ASNF(XX)=ATAN (XX/SQRT (1.0-XX**2))                               00067700
      DIMENSION DATA(50),NNEWA(50),LLCODE(50),LLVA(50),RES(50)          00067800
      SAMP=NODATA                                                       00067900
      DO 3 J=1,NVG                                                      00068000
  305 NEWA=NNEWA(J)                                                     00068100
      LCODE=LLCODE(J)                                                   00068200
      LVA=LLVA(J)                                                       00068300
      BNEW=RES(J)                                                       00068400
      IF(LCODE-10) 4,4,5                                                00068500
    5 NEWB=BNEW                                                         00068600
    4 D=DATA(LVA)                                                       00068700
      GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140              00068800
     1 ,150,160),LCODE                                                  00068900
   10 IF(D)99,7,8                                                       00069000
    7 DATA(NEWA)=0.0                                                    00069100
      GO TO 3                                                           00069200
    8 DATA(NEWA)=SQRT (D)                                               00069300
      GO TO 3                                                           00069400
   20 IF(D)99,11,12                                                     00069500
   11 DATA(NEWA)=1.0                                                    00069600
      GO TO 3                                                           00069700
   12 DATA(NEWA)=SQRT (D)+SQRT (D+1.0)                                  00069800
      GO TO 3                                                           00069900
   30 IF(D)99,99,14                                                     00070000
   14 DATA(NEWA)=ALOG10(D)                                              00070100
      GO TO 3                                                           00070200
   40 DATA(NEWA)=EXP (D)                                                00070300
      GO TO 3                                                           00070400
   50 IF(D)99, 7,17                                                     00070500
   17 IF(D-1.0)18,19,99                                                 00070600
   19 DATA(NEWA)=3.14159265/2.0                                         00070700
      GO TO 3                                                           00070800
   18 A=SQRT (D)                                                        00070900
      DATA(NEWA)=ASNF(A)                                                00071000
      GO TO 3                                                           00071100
   60 A=D/(SAMP+1.0)                                                    00071200
      B=A+1.0/(SAMP+1.0)                                                00071300
      IF(A)99,23,24                                                     00071400
   23 IF(B)99, 7,27                                                     00071500
   27 DATA(NEWA)=ASNF(SQRT (B))                                         00071600
      GO TO 3                                                           00071700
   24 IF(B)99,28,29                                                     00071800
   28 DATA(NEWA)=ASNF(SQRT (A))                                         00071900
      GO TO 3                                                           00072000
   29 A=SQRT (A)                                                        00072100
      B=SQRT (B)                                                        00072200
      DATA(NEWA)=ASNF(A)+ASNF(B)                                        00072300
      GO TO 3                                                           00072400
   70 IF(D)31,99,31                                                     00072500
   31 DATA(NEWA)=1.0/D                                                  00072600
      GO TO 3                                                           00072700
   80 DATA(NEWA)=D+BNEW                                                 00072800
      GO TO 3                                                           00072900
   90 DATA(NEWA)=D*BNEW                                                 00073000
      GO TO 3                                                           00073100
   33 DATA(NEWA)=D**BNEW                                                00073200
  100 IF(D)33, 7,33                                                     00073300
  120 DATA(NEWA)=D-DATA(NEWB)                                           00073400
      GO TO 3                                                           00073500
  110 DATA(NEWA)=D+DATA(NEWB)                                           00073600
      GO TO 3                                                           00073700
      GO TO 3                                                           00073800
  130 DATA(NEWA)=D*DATA(NEWB)                                           00073900
      GO TO 3                                                           00074000
  140 IF(DATA(NEWB))34,99,34                                            00074100
   34 DATA(NEWA)=D/DATA(NEWB)                                           00074200
      GO TO 3                                                           00074300
  150 BNEW=NEWB                                                         00074400
      IF(D-BNEW) 7,11,11                                                00074500
  160 IF(D-DATA(NEWB)) 7,11,11                                          00074600
    3 CONTINUE                                                          00074700
      GO TO 42                                                          00074800
   99 LCASE=-999                                                        00074900
      IF(MERRY-J) 402,401,402                                           00075000
  402 MERRY=J                                                           00075100
      WRITE            (6,1404)J                                        00075200
  401 WRITE            (6,1405)N                                        00075300
      WRITE            (6,1408)                                         00075400
      ISAMP=ISAMP+1                                                     00075500
 1408 FORMAT(45H0THIS CASE WILL BE DELETED FOR ALL VARIABLES )          00075600
 1404 FORMAT(30H0THE INSTRUCTIONS INDICATED ON/25H TRANS GENERATOR CARD 00075700
     1NO.I2,4H RE-/29H SULTED IN THE VIOLATION OF A/31H RESTRICTION FOR 00075800
     2THIS TRANSFOR-/31H MATION. THE VIOLATION OCCURRED/27H FOR THE CASE00075900
     3 LISTED BELOW./)                                                  00076000
 1405 FORMAT( 9H CASE NO.I5)                                            00076100
   42 RETURN                                                            00076200
      END                                                               00076300
                                                                        00076400
CVFCH      SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDS00076500
      SUBROUTINE VFCHCK(NVF)                                            00076600
      IF(NVF)10,10,20                                                   00076700
 10   WRITE            (6,4000)                                         00076800
      NVF=1                                                             00076900
 50   RETURN                                                            00077000
C                                                                       00077100
 20   IF(NVF-10)50,50,10                                                00077200
C                                                                       00077300
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00077400
     XIED, ASSUMED TO BE 1.)                                            00077500
      END                                                               00077600
