CID                     BMD02T                                                  
CID   0901HS 15 150    $BMD02T AUTOCOVARIANCE AND POWER SPECTRA ANALYSIST0200000
C     MAP                                                               T0200010
C     XEQ                                                               T0200020
C     LABEL                                                             T0200030
C     LIST8                                                             T0200040
C     FORTRAN                                                           T0200050
CBMD02T  AUTOCOVARIANCE AND POWER SPECTRAL ANALYSIS  OCTOBER 22, 1965   T0200060
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0200070
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0200080
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0200090
     3 CRO(400),COXYSQ(200),IPRNT1(20),IPRNT2(20)                       T0200100
      DIMENSION X(1000),Y(1000),SPX(200),SPY(200),IPRNT3(20)            T0200110
C                                                                       T0200120
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0200130
     -                              YP,SYM,CRO,COXYSQ,                  T0200140
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0200150
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0200160
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX ,Q       T0200170
     -,KPOINT                                                           T0200175
C                                                                       T0200180
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY),T0200190
     X(YES,IYES),(END,IEND),(PROBLM,IPROB),(SEL,ISEL)                   T0200200
C                                                                       T0200210
 9002 FORMAT(1H177H BMD02T-AUTOCOVARIANCE AND POWER SPECTRAL ANALYSIS - T0200220
     1VERSION OF OCT. 22, 1965/                                         T0200230
     22X40HHEALTH SCIENCES COMPUTING FACILITY, UCLA//)                  T0200240
C                                                                       T0200250
      END=6HFINISH                                                      T0200260
      PI=3.1415926536                                                   T0200270
      PROBLM=6HPROBLM                                                   T0200280
      SEL=6HSELECT                                                      T0200290
      YES=6HYES                                                         T0200300
C                                                                       T0200310
      INFORM=5                                                          T0200320
C                                                                       T0200330
 999  READ 5001,JOB,PROB,IORDAT,IPLDAT,IDETRN,IPREWT,THETA,             T0200340
     - KSER,NPOINT,LAG,NSEL,IPLOT,DELTAT,UNIT,IOLD,NTAPE,KVR            T0200350
 5001 FORMAT(2A6,4A3,F5.0,I2,I4,I3,2I2,F5.0,A6,A3,12X,2I2)              T0200360
      IF(IEND-JOB)10,888,10                                             T0200370
 888  IF(INFORM-5)890,890,889                                           T0200380
 889  CALL REMOVE(INFORM)                                               T0200390
 890  CALL EXIT                                                         T0200400
 10   IF(IPROB-JOB)740,150,740                                          T0200410
 150  PRINT 9002                                                        T0200420
      PRINT 9003,PROB                                                   T0200430
 9003 FORMAT(1H ///40X,26HP R O B L E M  N U M B E R,4X,A6/40X,26(1H*)//T0200440
     X/)                                                                T0200450
      PRINT 9005,IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,                T0200460
     - NPOINT,LAG,NSEL,IOLD                                             T0200470
 9005 FORMAT(20X,31HINPUT DATA TO BE PRINTED OUT---A3//20X,33HINPUT SERIT0200480
     XES TO BE PLOTTED OUT---A3//20X,13HDETRENDING---A3//20X,15HPREWHITET0200490
     XNING---A3//20X,81HVALUE OF CONSTANT C USED IN THE PREWHITENING TRAT0200500
     XNSFORMATION Z(T)=X(T+1)-CX(T) ---F10.5//20X19HNUMBER OF SERIES = IT0200510
     X2//20X,23HNUMBER OF DATA POINTS =I5//20X,24HNUMBER OF LAGS CHOSEN T0200520
     X= I3//20X,28HNUMBER OF SELECTION CARDS = I2//20X,20HUSE PREVIOUS DT0200530
     XATA---A3//)                                                       T0200540
      IF(KSER*(KSER-21))904,750,750                                     T0200545
 904  PRINT 9013,DELTAT,UNIT                                            T0200550
 9013 FORMAT(20X,25HCONSTANT TIME INTERVAL = F10.5,1X,A6//)             T0200560
      IF(NPOINT*(NPOINT-1001))909,760,760                               T0200565
 909  PRINT 9021,KVR                                                    T0200570
 9021 FORMAT(20X,34HNUMBER OF VARIABLE FORMAT CARDS = I3//)             T0200580
      KPOINT=NPOINT                                                     T0200585
      CTHETA=ABSF(THETA)                                                T0200590
      IF(LAG-199)8,8,730                                                T0200600
    8 IF((NPOINT*KSER)-17000)9,9,720                                    T0200610
 9    IF(CTHETA-1.0)99,710,710                                          T0200620
 99   IF(IOLD-IYES)11,30,11                                             T0200630
 11   ASSIGN 75 TO NSKIP                                                T0200640
 12   IF(KVR)40,705,20                                                  T0200650
 40   IF(KVR+1)42,45,42                                                 T0200660
 42   L=1                                                               T0200670
      ASSIGN 74 TO NSKIP                                                T0200680
      GO TO 46                                                          T0200690
 45   L=2                                                               T0200700
 46   KVR=1                                                             T0200710
      GO TO (25,20),L                                                   T0200720
 20   CALL VFCHCK(KVR)                                                  T0200730
      KVR=KVR*12                                                        T0200740
      READ 1002,(FMT(I),I=1,KVR)                                        T0200750
 1002 FORMAT(12A6)                                                      T0200760
 25   ASSIGN 80 TO KSKIP                                                T0200770
      ASSIGN 30 TO LSKIP                                                T0200780
      IF ( NTAPE) 71,72,73                                              T0200790
 71   KADD=-NPOINT                                                      T0200800
       DO 84 K=1,KSER                                                   T0200810
      KADD=KADD+NPOINT                                                  T0200820
      GO TO NSKIP,(74,75,715)                                           T0200830
 715  READ(INFORM) (X(I),I=1,NPOINT)                                    T0200840
      GO TO 755                                                         T0200850
C                                                                       T0200860
 74   READ 1002,(FMT(J),J=1,12)                                         T0200870
 75   READ FMT,(X(I),I=1,NPOINT)                                        T0200880
  755 DO 84 I=1,NPOINT                                                  T0200890
      ND=I+KADD                                                         T0200900
   84 Z(ND)=X(I)                                                        T0200910
      GO TO 825                                                         T0200920
C                                                                       T0200930
   73 ASSIGN 83 TO LSKIP                                                T0200940
      IF(70- NTAPE)735,735,738                                          T0200950
 735  NTAPE=NTAPE-70                                                    T0200960
      ASSIGN 715 TO NSKIP                                               T0200970
      CALL TPWD(NTAPE,INFORM)                                           T0200980
      GO TO 71                                                          T0200990
C                                                                       T0201000
 738  CALL TPWD(NTAPE,INFORM)                                           T0201010
      ASSIGN 81 TO KSKIP                                                T0201020
   72 DO 82 I=1,NPOINT                                                  T0201030
      KADD=0                                                            T0201040
      GO TO KSKIP,(80,81)                                               T0201050
 80   READ FMT,(DUMB(J),J=1,KSER)                                       T0201060
      GO TO 815                                                         T0201070
C                                                                       T0201080
 81   READ(INFORM)(DUMB(J),J=1,KSER)                                    T0201090
 815  DO 82 J=1,KSER                                                    T0201100
      L=KADD+I                                                          T0201110
      KADD=KADD+NPOINT                                                  T0201120
   82 Z(L)=DUMB(J)                                                      T0201130
  825 GO TO LSKIP,(30,83)                                               T0201140
 83   REWIND INFORM                                                     T0201150
   30 LLAG=LAG+1                                                        T0201160
      FLLAG=LLAG                                                        T0201170
      FLAG=LAG                                                          T0201180
      FNPT=NPOINT                                                       T0201190
      Q=PI/FLAG                                                         T0201200
      CONST=2.0*DELTAT/PI                                               T0201210
      PMD=0.5/(FLAG*DELTAT)                                             T0201220
      IF(-NSEL)95,770,770                                               T0201225
   95 DO 9000 KDUM=1,NSEL                                               T0201230
      KIT=0                                                             T0201240
      DO 98 I=1,20                                                      T0201250
      NSELL(I)=0                                                        T0201260
      IPRNT1(I)=0                                                       T0201270
      IPRNT2(I)=0                                                       T0201280
      IPRNT3(I)=0                                                       T0201290
      LISA(I)=0                                                         T0201300
 98   CONTINUE                                                          T0201310
      READ 5003, JOB,IPOW,ICCS,ICOH,NNX,IPRNT1(1),LISA(1),NN            T0201320
     XS,(NSELL(I),IPRNT1(I+1),IPRNT2(I+1),IPRNT3(I+1),LISA(I+1),I=1,NNS)T0201330
 5003 FORMAT(A6,1X,3A4,I2,A1,2I2,1X,6(I2,3A1,I2)/6X,9(I2,3A1,I2)/6X,    T0201340
     X4(I2,3A1,I2))                                                     T0201345
      IF(JOB-ISEL)740,101,740                                           T0201350
 101  CALL SMEAN (NNX,X,XBAR,XALPHA)                                    T0201360
 102  IF (MISTAK) 9000,100,9000                                         T0201380
 100  ASSIGN 145 TO KPOWR                                               T0201390
      IF(IPOW-IYES)105,110,105                                          T0201400
 110  ASSIGN 135 TO KPOWR                                               T0201410
      CALL POWER (NNX,X,XBAR,XALPHA,SPX)                                T0201420
  105 DO 8000 I=1,NNS                                                   T0201430
      KIT=I                                                             T0201440
      NNY=NSELL(I)                                                      T0201450
      IF(NNX-NNY)120,9000,120                                           T0201460
  120 CALL SMEAN (NNY,Y,YBAR,YALPHA)                                    T0201470
      IF(MISTAK-17)122,740,122                                          T0201480
 122  IF(MISTAK) 9000,125,9000                                          T0201490
 125  GO TO KPOWR,(135,145)                                             T0201500
  135 CALL POWER (NNY,Y,YBAR,YALPHA,SPY)                                T0201510
      IF(ICCS-IYES)8000,140,8000                                        T0201520
 140  IF(IPREWT-IYES)145,130,145                                        T0201530
  145 CALL CROSS (NNX,NNY,X,Y,SPX,SPY,XBAR,YBAR,XALPHA,YALPHA)          T0201540
      GO TO 8000                                                        T0201550
 130  PRINT 1051                                                        T0201560
 1051 FORMAT(1H1,10X,77HSINCE PREWHITENING IS DONE IN THE INPUT DATA , CT0201570
     -ROSS-SPECTRUM IS NOT COMPUTED/ 10X,66HPROGRAM WILL PROCEED TO NEXTT0201580
     - SERIES IN THE SELECTION CARD (IF ANY)/ 10X,39H OR TO THE NEXT SELT0201590
     -ECTION CARD (IF ANY))                                             T0201600
 8000 CONTINUE                                                          T0201610
 9000 CONTINUE                                                          T0201620
      GO TO 999                                                         T0201630
C                                                                       T0201640
  705 IF(INFORM)20,20,73                                                T0201650
C                                                                       T0201660
 710  PRINT 9200,THETA                                                  T0201670
      GO TO 751                                                         T0201680
C                                                                       T0201690
 720  PRINT 9300                                                        T0201700
      GO TO 751                                                         T0201710
C                                                                       T0201720
 730  PRINT 9400,LAG                                                    T0201730
      GO TO 751                                                         T0201740
C                                                                       T0201750
 740  PRINT 9500                                                        T0201760
      GO TO 751                                                         T0201762
C                                                                       T0201764
 750  PRINT 9600                                                        T0201766
 751  PRINT 9800                                                        T0201768
      GO TO 888                                                         T0201770
C                                                                       T0201772
 760  PRINT 9700                                                        T0201774
      GO TO 751                                                         T0201776
C                                                                       T0201778
 770  PRINT 9900                                                        T0201780
      NSEL =1                                                           T0201782
      GO TO 95                                                          T0201784
C                                                                       T0201786
 9200 FORMAT(1H0,20X,47HCONSTANT USED IN PREWHITENING TRANSFORMATION IS,T0201790
     XF7.5,23H,A VALUE NOT PERMITTED.)                                  T0201800
 9300 FORMAT(1H0,37X,22HDATA STORAGE EXCEEDED.)                         T0201810
 9400 FORMAT(1H0,37X,29HNUMBER OF LAGS IS TOO LARGE =,I5)               T0201820
 9500 FORMAT(1H0,37X,27HCONTROL CARDS OUT OF ORDER.)                    T0201830
 9600 FORMAT(1H0,37X,43HNUMBER OF SERIES IS OUTSIDE PROGRAM LIMITS.)    T0201832
 9700 FORMAT(1H0,37X,43HNUMBER OF POINTS IS OUTSIDE PROGRAM LIMITS.)    T0201834
 9800 FORMAT(1H0,37X,23HPROGRAM CANNOT PROCEED.)                        T0201840
 9900 FORMAT(1H0,37X,44HNO SELECTION CARD SPECIFIED. ONE IS ASSUMED.)   T0201850
C                                                                       T0201860
      END                                                               T0201870
C     LABEL                                                             T0201880
C     LIST8                                                             T0201890
C     FORTRAN                                                           T0201900
CCROSS        SUBROUTINE CROSS FOR BMD02T             JULY 22, 1964     T0201910
      SUBROUTINE CROSS(NNX,NNY,X,Y,SPX,SPY,XBAR,YBAR,XALPHA,YALPHA)     T0201920
C                                                                       T0201930
      EMDAF(FN,H,R)=((FN*H)**2)*(1.0-2.0*(R/FN)-2.0*(R/FN)**2)          T0201940
C                                                                       T0201950
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0201960
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0201970
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0201980
     3 CRO(400),COXYSQ(200),IPRNT1(20),IPRNT2(20)                       T0201990
      DIMENSION X(1000),Y(1000),SPX(200),SPY(200),IPRNT3(20)            T0202000
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0202010
     -                              YP,SYM,CRO,COXYSQ,                  T0202020
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0202030
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0202040
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX ,Q       T0202050
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY),T0202060
     X(YES,IYES),(B,IB),(C,IC),(SS,IS),(H,IH),(T,IT)                    T0202070
C                                                                       T0202080
      B=6HB                                                             T0202090
      BLANK=6H                                                          T0202100
      C=6HC                                                             T0202110
      DBLSTR=6H**                                                       T0202120
      H=6HH                                                             T0202130
      SS=6HS                                                            T0202140
      STAR=6HX                                                          T0202150
      SYM(1)=6H*00000                                                   T0202160
      T=6HT                                                             T0202170
      YES=6HYES                                                         T0202180
C                                                                       T0202190
      DO 501 I=1,LLAG                                                   T0202200
      SXY=0.0                                                           T0202210
      SYX=0.0                                                           T0202220
      M=I-1                                                             T0202230
      L=NPOINT-M                                                        T0202240
      FL=L                                                              T0202250
      DO 502 J=1,L                                                      T0202260
      K=M+J                                                             T0202270
      SXY=SXY+X(J)*Y(K)                                                 T0202280
  502 SYX=SYX+X(K)*Y(J)                                                 T0202290
      RXY(I)=SXY/FL                                                     T0202300
  501 RYX(I)=SYX/FL                                                     T0202310
      IF(IDETRN-IYES)455,450,455                                        T0202320
  450 FMGM=XBAR*YBAR                                                    T0202330
      CAP1=(XBAR*YALPHA-YBAR*XALPHA)/2.0                                T0202340
      CAP=-CAP1                                                         T0202350
      WARN1=(XALPHA*YALPHA)/12.0                                        T0202360
      FI=-1.0                                                           T0202370
      DO 504 I=1,LLAG                                                   T0202380
      FI=FI+1.0                                                         T0202390
      FLAMDA=EMDAF(FNPT,DELTAT,FI)                                      T0202400
      CAP=CAP+CAP1                                                      T0202410
      WARNER=FMGM+FLAMDA*WARN1                                          T0202420
      RXY(I)=RXY(I)-CAP-WARNER                                          T0202430
 504  RYX(I)=RYX(I)+CAP-WARNER                                          T0202440
 455  PRINT 1501,NNX,NNY,NNX,NNY,UNIT                                   T0202450
 1501 FORMAT(1H1,21X,3HLAG,14X,15HCROSSCOVARIANCE,15X,15HCROSSCOVARIANCET0202460
     X/37X,9HOF SERIES,I3,4H AND,I3,11X,9HOF SERIES,I3,4H AND,I3/20X,1H(T0202470
     XA6,1H),11X,15H(POSITIVE  TAU),15X,15H(NEGATIVE  TAU)//)           T0202480
      TIME=-DELTAT                                                      T0202490
      DO 503 I=1,LLAG                                                   T0202500
      TIME=TIME+DELTAT                                                  T0202510
 503  PRINT 1502,TIME,RXY(I),RYX(I)                                     T0202520
 1502 FORMAT(19X,F9.4,10X,F15.6,15X,F15.6)                              T0202530
      TIME1=TIME                                                        T0202540
  509 DO510 I=1,LLAG                                                    T0202550
      K=LLAG-I+1                                                        T0202560
  510 CRO(I)=RXY(K)                                                     T0202570
      KL=LLAG                                                           T0202580
      DO 511 I=2,LLAG                                                   T0202590
      KL=KL+1                                                           T0202600
  511 CRO(KL)=RYX(I)                                                    T0202610
      LLLAG=2*LLAG-1                                                    T0202620
      IF(IPRNT2(KIT+1)-IB)515,520,515                                   T0202630
 515  IF(IPRNT2(KIT+1)-IC)655,520,655                                   T0202640
 520  FR=-10.0**10                                                      T0202650
      RO=10**10                                                         T0202660
      DO630 I=1,LLLAG                                                   T0202670
      FR=MAX1F(FR,CRO(I))                                               T0202680
  630 RO=MIN1F(RO,CRO(I))                                               T0202690
      PRINT 1503,NNX,NNY,TIME1,UNIT                                     T0202730
 1503 FORMAT(1H1, 7X,44HGRAPH OF CROSS-COVARIANCE FUNCTION OF SERIES,I3,T0202740
     X4H AND,I3,36H PLOTTED AGAINST TIME UP TO A LAG OF,F9.4,1X,A6//)   T0202750
      ANY=-TIME1-DELTAT                                                 T0202760
      IF(LLAG-50) 640,645,645                                           T0202770
 640  K1=0                                                              T0202780
      K2=1                                                              T0202790
      ASSIGN 642 TO KSKIP                                               T0202800
      GO TO 641                                                         T0202810
C                                                                       T0202820
 645  K1=-1                                                             T0202830
      K2=K1                                                             T0202840
      ASSIGN 643 TO KSKIP                                               T0202850
      KZ=LLLAG+1                                                        T0202860
 641   DO 650 I=1,LLLAG                                                 T0202870
      ANY=ANY+DELTAT                                                    T0202880
      GO TO KSKIP,(642,643)                                             T0202890
 642  YP(1)=CRO(I)                                                      T0202900
      GO TO 650                                                         T0202910
C                                                                       T0202920
 643  KZ=KZ-1                                                           T0202930
      YP(1)=CRO(KZ)                                                     T0202940
 650  CALL PLOTR(ANY,-TIME1,TIME1,YP,SYM,RO,FR,1,K2)                    T0202950
      CALL PLOTR(ANY,-TIME1,TIME1,YP,SYM,RO,FR,K1,K2)                   T0202960
  655 DO 15 JP=1,LLAG                                                   T0202970
      ADDXY(JP)=RXY(JP)+RYX(JP)                                         T0202980
   15 SUBXY(JP)=RXY(JP)-RYX(JP)                                         T0202990
      ADDXY(1)=0.5*ADDXY(1)                                             T0203000
      ADDXY(LLAG)=0.5*ADDXY(LLAG)                                       T0203010
      SUBXY(1)=0.5*SUBXY(1)                                             T0203020
      SUBXY(LLAG)=0.5*SUBXY(LLAG)                                       T0203030
      S1=-Q                                                             T0203040
      DO 17 IH=1,LLAG                                                   T0203050
      CXY (IH)=0.0                                                      T0203060
      QXY(IH)=0.0                                                       T0203070
      S1=S1+Q                                                           T0203080
      S=-S1                                                             T0203090
      DO 17 JP=1,LLAG                                                   T0203100
      S=S+S1                                                            T0203110
      CXY(IH)=CXY(IH)+COSF(S)*ADDXY(JP)                                 T0203120
   17 QXY(IH)=QXY(IH)+SINF(S)*SUBXY(JP)                                 T0203130
      DO 16 I=1,LLAG                                                    T0203140
      CXY (I)=CXY(I)*CONST *.5                                          T0203150
   16 QXY( I)=QXY(I)*CONST*.5                                           T0203160
      SCXY(1)=.54*CXY(1)+.46*CXY(2)                                     T0203170
      SCXY(LLAG)=.54*CXY(LLAG)+.46*CXY(LLAG-1)                          T0203180
      SQXY(1)=.54*QXY(1)+.46*QXY(2)                                     T0203190
      SQXY(LLAG)=.54*QXY(LLAG)+.46*QXY(LLAG-1)                          T0203200
      KK=LLAG-1                                                         T0203210
      DO 18 J=2,KK                                                      T0203220
      SCXY(J)=.54*CXY(J)+.23*(CXY(J+1)+CXY(J-1))                        T0203230
   18 SQXY(J)=.54*QXY(J)+.23*(QXY(J+1)+QXY(J-1))                        T0203240
      DO 19 J=1,LLAG                                                    T0203250
   19 AMXY(J)=SQRTF(SCXY(J)**2+SQXY(J)**2)                              T0203260
      CALL HONG                                                         T0203270
      PRINT  1005,UNIT,NNX,NNY,NNX,NNY,NNX,NNY,NNX,NNY                  T0203280
 1005 FORMAT(12H1  FREQUENCY,6X,11HCO-SPECTRUM,6X,19HQUADRATURE SPECTRUMT0203290
     1,5X,27HAMPLITUDE OF CROSS-SPECTRUM,4X,23HPHASE OF CROSS-SPECTRUM/1T0203300
     XX,8H(CYCLES/A6,1H),6X,2HOF,19X,2HOF,26X,2HOF,27X,2HOF/16X,6HSERIEST0203310
     X,I3,4H AND,I3,5X,6HSERIES,I3,4H AND,I3,12X,6HSERIES,I3,4H AND,I3,1T0203320
     X3X,6HSERIES,I3,4H AND,I3,2H *//)                                  T0203330
      ANY=-PMD                                                          T0203340
      DO 22 I=1,LLAG                                                    T0203350
      ANY=ANY+PMD                                                       T0203360
 22   PRINT  1006,ANY,SCXY(I),SQXY(I),AMXY(I),PHASXY(I)                 T0203370
 1006 FORMAT(3X,F8.3,5X,F14.7,7X,F14.7,14X,F14.7,15X,F14.7)             T0203380
      PRINT 543                                                         T0203390
  543 FORMAT(1H0,10X,37H* PHASE GIVEN IN FRACTION OF A CIRCLE)          T0203400
      IF(IPRNT2(KIT+1)-IB)201,204,201                                   T0203410
 201  IF(IPRNT2(KIT+1)-IS)208,204,208                                   T0203420
 204  DO 205 I=1,LLAG                                                   T0203430
  205 SCXY(I)=LOGF(AMXY(I))                                             T0203440
      PRINT 1010,NNX,NNY,UNIT                                           T0203450
 1010 FORMAT(1H1, 8X,46HGRAPH OF AMPLITUDE OF CROSS-SPECTRUM OF SERIES,IT0203460
     X3,4H ANDI3,42H (IN LOG SCALE) AGAINST FREQUENCY (CYCLES/A6,1H)//) T0203470
      CALL PLUG (SCXY)                                                  T0203480
      PRINT 1011,NNX,NNY,UNIT                                           T0203490
 1011 FORMAT(1H1,3X,33HPHASE OF CROSS-SPECTRUM OF SERIES,I3,4H AND,I3,63T0203500
     XH -- PLOTTED IN FRACTIONS OF A CIRCLE AGAINST FREQUENCY (CYCLES/,AT0203510
     X6,1H)//)                                                          T0203520
      CALL KONG                                                         T0203530
 208  IF(ICOH-IYES)777,215,777                                          T0203540
 215  IF(IPOW-IYES)210,216,210                                          T0203550
 210  PRINT 1015,NNX,NNY,KDUM                                           T0203560
 1015 FORMAT(1H1,10X,5H*****/15X,29HTHE POWER SPECTRUM OF SERIES I3,5H AT0203570
     -ND I3,54H ARE NOT COMPUTED ACCORDING TO THIS SELECTION CARD NO. I3T0203580
     -/15X,68HTHE COHERENCE SQUARE AND THE TRANSFER FUNCTIONS CANNOT BE T0203590
     -CALCULATED/ 15X,5H*****)                                          T0203600
      GO TO 777                                                         T0203610
C                                                                       T0203620
  216 ASSIGN 224 TO KADD1                                               T0203630
      ASSIGN 250 TO KADD2                                               T0203640
      SMAX=-9999999.0                                                   T0203650
      SCXMAX=-9999999.0                                                 T0203660
      SQXMAX=-9999999.0                                                 T0203670
      DO 220 I=1,LLAG                                                   T0203680
      IF (SPX(I))212,214,212                                            T0203690
  212 RX(I)=BLANK                                                       T0203700
      SQXY(I)=AMXY(I)/SPX(I)                                            T0203710
      SQXMAX=MAX1F(SQXMAX,SQXY(I))                                      T0203720
  213 IF(SPY(I))217,218,217                                             T0203730
  217 CRO(I)=BLANK                                                      T0203740
      COXYSQ(I)=(AMXY(I)**2)/(SPX(I)*SPY(I))                            T0203750
      IF(1.1-COXYSQ(I))2175,2180,2180                                   T0203760
 2175 CRO(I)=DBLSTR                                                     T0203770
      ASSIGN 240 TO KADD2                                               T0203780
      GO TO 221                                                         T0203790
C                                                                       T0203800
 2180 SMAX=MAX1F(SMAX,COXYSQ(I))                                        T0203810
      GO TO 221                                                         T0203820
C                                                                       T0203830
  214 RX(I)=STAR                                                        T0203840
      ASSIGN 223 TO KADD1                                               T0203850
      COXYSQ(I)=0.0                                                     T0203860
      CRO(I)=STAR                                                       T0203870
      SQXY(I)=0.0                                                       T0203880
  219 IF(SPY(I))221,222,221                                             T0203890
  221 PX(I)=BLANK                                                       T0203900
      SCXY(I)=AMXY(I)/SPY(I)                                            T0203910
      SCXMAX=MAX1F(SCXMAX,SCXY(I))                                      T0203920
      GO TO 220                                                         T0203930
C                                                                       T0203940
  218 COXYSQ(I)=0.0                                                     T0203950
      CRO(I)=STAR                                                       T0203960
  222 PX(I)=STAR                                                        T0203970
      SCXY(I)=0.0                                                       T0203980
      ASSIGN 223 TO KADD1                                               T0203990
  220 CONTINUE                                                          T0204000
      PRINT 1012,UNIT,NNX,NNY,NNX,NNY,NNY,NNX,NNX,NNY                   T0204010
 1012 FORMAT(13H1   FREQUENCY7X16HCOHERENCE SQUARE,11X,12HAMPLITUDE OF,1T0204020
     X3X,12HAMPLITUDE OF,15X,8HPHASE OF/9H (CYCLES/A6,1H),11X,2HOF,15X,3T0204030
     X(18HTRANSFER  FUNCTION7X)/20X,6HSERIES,I3,4H AND,I3,11X2(4HFROMI3,T0204040
     X3H TO,I3,11X)4HFROMI3,3H TO,I3,2H *//)                            T0204050
      ANY=-PMD                                                          T0204060
      DO 230 I=1,LLAG                                                   T0204070
      ANY=ANY+PMD                                                       T0204080
      PRINT 1016,ANY,COXYSQ(I),CRO(I),SQXY(I),RX(I),SCXY(I              T0204090
     X),PX(I),PHASXY(I)                                                 T0204100
 1016 FORMAT(3X,F9.4,9X,F14.7,3(A2,9X,F14.7))                           T0204110
      IF(CRO(I)-DBLSTR)227,228,227                                      T0204120
  227 IF(CRO(I)-STAR)229,228,229                                        T0204130
  228 COXYSQ(I)=SMAX                                                    T0204140
  229 IF(RX(I)-STAR)2291,2292,2291                                      T0204150
 2291 IF(PX(I)-STAR)230,2294,230                                        T0204160
 2292 SQXY(I)=SQXMAX                                                    T0204170
      GO TO 2291                                                        T0204180
C                                                                       T0204190
 2294 SCXY(I)=SCXMAX                                                    T0204200
  230 CONTINUE                                                          T0204210
      PRINT 543                                                         T0204220
      GO TO KADD2,(240,250)                                             T0204230
 240  PRINT 1017                                                        T0204240
 1017 FORMAT(1H0,10X,84H** INDICATES THAT THIS VALUE IS TOO HIGH DUE TO T0204250
     XSAMPLING ERROR. IT WILL BE SET EQUAL/14X,71HTO THE MAXIMUM VALUE OT0204260
     XF THE REMAINING COHERENCES FOR PLOTTING PURPOSES.)                T0204270
  250 GO TO KADD1,(223,224)                                             T0204280
 223  PRINT 1018                                                        T0204290
 1018 FORMAT(1H0,10X,91HX INDICATES THIS VALUE IS NOT COMPUTABLE DUE TO T0204300
     XA NEGATIVE OR ZERO POWER SPECTRAL ESTIMATE./13X,82HIT WILL BE SET T0204310
     XEQUAL TO THE MAXIMUM OF THE REMAINING VALUES FOR PLOTTING PURPOSEST0204320
     X.)                                                                T0204330
 224  IF(IPRNT3(KIT+1)-IB)231,232,231                                   T0204340
 231  IF(IPRNT3(KIT+1)-IH)233,232,233                                   T0204350
 232  PRINT 1013,NNX,NNY,UNIT                                           T0204360
 1013 FORMAT(1H1,16X,40HGRAPH OF COHERENCE SQUARE BETWEEN SERIESI3,4H ANT0204370
     XDI3,27H AGAINST FREQUENCY (CYCLES/A6,1H)//)                       T0204380
      CALL PLUG (COXYSQ)                                                T0204390
 233  IF(IPRNT3(KIT+1)-IB)234,235,234                                   T0204400
 234  IF(IPRNT3(KIT+1)-IT)777,235,777                                   T0204410
 235  DO 225 I=1,LLAG                                                   T0204420
      SQXY(I)=LOGF(SQXY(I))                                             T0204430
  225 SCXY(I)=LOGF(SCXY(I))                                             T0204440
      PRINT 1014,NNX,NNY,UNIT                                           T0204450
 1014 FORMAT(1H1, 5X,51HGRAPH OF AMPLITUDE OF TRANSFER FUNCTION FROM SERT0204460
     XIESI3,3H TOI3,42H (IN LOG SCALE) AGAINST FREQUENCY (CYCLES/A6,1H)/T0204470
     X/)                                                                T0204480
      CALL PLUG (SQXY)                                                  T0204490
      PRINT 1014,NNX,NNY,UNIT                                           T0204500
      CALL PLUG (SCXY)                                                  T0204510
  777 RETURN                                                            T0204520
C                                                                       T0204530
      END                                                               T0204540
      SUBROUTINE FORM2(T,M,SYMB)                                        T0204550
                                                                        T0204560
C      SUBROUTINE FORM2 OF PLOTR                                        T0204570
C      ORIGINALLY WRITTEN BY RICHARD KRONMAL                            T0204580
C      MODIFIED MAR. 22, 1964 BY EUGENE ALBRIGHT,                       T0204590
C              HEALTH SCIENCES COMPUTING FACILITY, UCLA                 T0204600
C     REWRITTEN BY DONNA WILLIAMS OF D.R.I. IN FORTRAN. APR,1968        T0204610
C                                                                       T0204620
C      CALLING SEQUENCE  CALL FORM2(T,M,SYMB)                           T0204630
C                                                                       T0204640
C                                                                       T0204650
      ISYM=CONCAT(T,0,1,1,11)                                           T0204660
      ISYM=ISYM*2**M                                                    T0204670
      ISYM=CONCAT(0,ISYM,12,12,6)                                       T0204680
      IMASK=6HI00000                                                    T0204690
      BLANK=6H 00000                                                    T0204700
      AMASK=6HA00000                                                    T0204710
      PMASK=6H+00000                                                    T0204720
      SMASK=6H/00000                                                    T0204730
      ONE  =6H100000                                                    T0204740
      TWO  =6H200000                                                    T0204750
      NINE =6H900000                                                    T0204760
      IF (ISYM-SMASK) 100,900,800                                       T0204770
 100  IF (ISYM-BLANK) 300,200,900                                       T0204780
 200  ISYM=CONCAT(0,SYMB,12,12,6)                                       T0204790
                                                                        T0204800
      GO TO 1000                                                        T0204810
 300  IF (ISYM-NINE) 400,500,600                                        T0204820
 400  ISYM=ISYM+ONE                                                     T0204830
      GO TO 1000                                                        T0204840
 500  ISYM=AMASK                                                        T0204850
      GO TO 1000                                                        T0204860
 600  IF (ISYM-IMASK) 700,650,800                                       T0204870
 650  ISYM=SMASK                                                        T0204880
      GO TO 1000                                                        T0204890
 700  IF(ISYM-PMASK)  800,800,400                                       T0204900
 800  ISYM=TWO                                                          T0204910
 1000 ISYM=ISYM/(2**M)                                                  T0204920
      T=ISYM                                                            T0204930
 900  CONTINUE                                                          T0204940
      RETURN                                                            T0204945
      END                                                               T0204950
      REAL FUNCTION   LOG10F(A)                                         T0204960
      LOG10F=ALOG10(A)                                                  T0204970
      RETURN                                                            T0204980
      END                                                               T0204990
      REAL FUNCTION   EXPF(A)                                           T0205000
      EXPF=EXP(A)                                                       T0205010
      RETURN                                                            T0205020
      END                                                               T0205030
      INTEGER FUNCTION   XMODF(I,J)                                     T0205040
      XMODF=MOD(I,J)                                                    T0205050
      RETURN                                                            T0205060
      END                                                               T0205070
C     LABEL                                                             T0205080
C     LIST8                                                             T0205090
C     FORTRAN                                                           T0205100
CHONG    SUBROUTINE HONG FOR BMD02T                  APRIL 15, 1963     T0205110
      SUBROUTINE HONG                                                   T0205120
C                                                                       T0205130
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0205140
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0205150
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0205160
     3 CRO(400),COXYSQ(200),IPRNT1(20),IPRNT2(20),IPRNT3(20)            T0205170
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0205180
     -                              YP,SYM,CRO,COXYSQ,                  T0205190
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0205200
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0205210
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX          T0205220
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY) T0205230
C                                                                       T0205240
C     PI2 IS EQUAL TO 2 TIMES PI                                        T0205250
C                                                                       T0205260
      PI2=2*3.1415926536                                                T0205270
C                                                                       T0205280
      DO 10 I=1,LLAG                                                    T0205290
      AB=ABSF(SQXY(I)/SCXY(I))                                          T0205300
      PHI=ATANF(AB)                                                     T0205310
      IF( SCXY(I)) 11,12,13                                             T0205320
   11 IF(SQXY(I)) 17,30,18                                              T0205330
   17 PHASXY(I)=PI+PHI                                                  T0205340
      GO TO 10                                                          T0205350
C                                                                       T0205360
 30   PHASXY(I)=PI                                                      T0205370
      GO TO 10                                                          T0205380
C                                                                       T0205390
   18 PHASXY(I)=PI-PHI                                                  T0205400
      GO TO 10                                                          T0205410
C                                                                       T0205420
 12   IF(SQXY(I))35,15,40                                               T0205430
 35   PHASXY(I)=1.5*PI                                                  T0205440
      GO TO 10                                                          T0205450
C                                                                       T0205460
   15 PHASXY(I)=0.0                                                     T0205470
      GO TO 10                                                          T0205480
C                                                                       T0205490
C     STATEMENT 40 SETS PHASXY(I) EQUAL TO PI DIVIDED BY 2              T0205500
C                                                                       T0205510
 40   PHASXY(I)=3.1415926536/2                                          T0205520
      GO TO 10                                                          T0205530
C                                                                       T0205540
   13 IF (SQXY(I)) 14,15,16                                             T0205550
 14   PHASXY(I)=PI2-PHI                                                 T0205560
      GO TO 10                                                          T0205570
C                                                                       T0205580
   16 PHASXY(I)=PHI                                                     T0205590
   10 CONTINUE                                                          T0205600
      DO 100 I=1,LLAG                                                   T0205610
  100 PHASXY(I)=PHASXY(I)/PI2                                           T0205620
      RETURN                                                            T0205630
C                                                                       T0205640
      END                                                               T0205650
C     LABEL                                                             T0205660
C     LIST8                                                             T0205670
C     FORTRAN                                                           T0205680
CKONG    SUBROUTINE KONG FOR BMD02T               FEBRUARY 17, 1964     T0205690
      SUBROUTINE KONG                                                   T0205700
C                                                                       T0205710
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0205720
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0205730
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0205740
     3 CRO(400),COXYSQ(200),IPRNT1(20),IPRNT2(20),IPRNT3(20)            T0205750
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0205760
     -                              YP,SYM,CRO,COXYSQ,                  T0205770
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0205780
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0205790
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX          T0205800
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY) T0205810
C                                                                       T0205820
      SYM(3)=6H100000                                                   T0205830
      SYM(4)=SYM(3)                                                     T0205840
C                                                                       T0205850
      IF(LLAG-35) 10,10,15                                              T0205860
   10 NTIMES=2                                                          T0205870
      GO TO 30                                                          T0205880
   15 NTIMES=1                                                          T0205890
   30 TIMES=NTIMES+1                                                    T0205900
      DX=PMD/TIMES                                                      T0205910
      XMAX=1.0/(2.0*DELTAT)                                             T0205920
      YMAX=1.5                                                          T0205930
      YMIN=-.5                                                          T0205940
      YP(3)=0.0                                                         T0205950
      YP(4)=1.0                                                         T0205960
      BNY=-PMD                                                          T0205970
      DO 100 I=1,LLAG                                                   T0205980
      BNY=BNY+PMD                                                       T0205990
      ANY=BNY                                                           T0206000
      FACE=PHASXY(I)+1.0                                                T0206010
      IF( FACE-1.5) 105,105,110                                         T0206020
  110 FACE=FACE-2.0                                                     T0206030
  105 YP(1)=PHASXY(I)                                                   T0206040
      YP(2)=FACE                                                        T0206050
      SYM(1)=6HX00000                                                   T0206060
      SYM(2)=6H*00000                                                   T0206070
      CALL PLOTR (ANY,0.0,XMAX,YP,SYM,YMIN,YMAX,4,-1)                   T0206080
      SYM(1)=6H 00000                                                   T0206090
      SYM(2)=SYM(1)                                                     T0206100
  125 DO 200 J=1,NTIMES                                                 T0206110
      ANY=ANY+DX                                                        T0206120
  200 CALL PLOTR (ANY,0.0,XMAX,YP,SYM,YMIN,YMAX, 4,-1)                  T0206130
  100 CONTINUE                                                          T0206140
      CALL PLOTR (ANY,0.0,XMAX,YP,SYM,YMIN,YMAX,-1,-1)                  T0206150
      RETURN                                                            T0206160
C                                                                       T0206170
      END                                                               T0206180
C     LABEL                                                             T0206190
C     LIST8                                                             T0206200
C     FORTRAN                                                           T0206210
CPLOTR        SUBROUTINE PLOTR                        JULY 20, 1964     T0206220
      SUBROUTINE PLOTR(X,ZMIN,ZMAX,Y,SYM,WMIN,WMAX,NC,NP)               T0206230
C                                                                       T0206232
C     PLOTR WAS MODIFIED THIS DATE TO GIVE BETTER SCALES.               T0206234
C                                                                       T0206236
      COMMON /A/ NCC,IC,XY,XMAX,YMAX,XR,YR,XM,TP,TC,XIJ,YIJ,JX,JY       T0206237
     -,GF,FMT,YMIN,XMIN                                                 T0206238
      DIMENSION XY(51,17),Y(15),CLAB(12),XM(6),SYM(15),GF(10),FMT(10)   T0206240
C                                                                       T0206250
 100   FORMAT(1H 6X5(F12.3,8X),F12.3/17X,5(F12.3,8X))                   T0206260
 101  FORMAT(1H F12.3,1X,A1,16A6,A5,A1,I2)                              T0206270
  102 FORMAT(1H 13X,A1,16A6,A5,A1)                                      T0206275
 1000 FORMAT(1H  14X,101A1)                                             T0206280
 1001 FORMAT(15X,20(5H+....),1H+)                                       T0206285
      BLANKS=(+6H      )                                                T0206287
      IF(NCC)48,50,48                                                   T0206290
   50 KL=0                                                              T0206300
      XM(1)=6H"00000                                                    T0206302
      XM(2)=6H0"0000                                                    T0206304
      XM(3)=6H00"000                                                    T0206306
      XM(4)=6H000"00                                                    T0206308
      XM(5)=6H0000"0                                                    T0206310
      XM(6)=6H00000"                                                    T0206312
      GF(1)=(+6H1X    )                                                 T0206314
      GF(2)=(+6H2X    )                                                 T0206316
      GF(3)=(+6H3X    )                                                 T0206318
      GF(4)=(+6H4X    )                                                 T0206320
      GF(5)=(+6H5X    )                                                 T0206322
      GF(6)=(+6H6X    )                                                 T0206324
      GF(7)=(+6H7X    )                                                 T0206326
      GF(8)=(+6H8X    )                                                 T0206328
      GF(9)=(+6H9X    )                                                 T0206330
      GF(10)=(+6H10X   )                                                T0206332
      FMT(1)=(+6H(17X  )                                                T0206334
      FMT(2)=BLANKS                                                     T0206336
      FMT(3)=BLANKS                                                     T0206338
      FMT(4)=(+6H5(F12.)                                                T0206340
      FMT(5)=(+6H3,8X)/)                                                T0206342
      FMT(6)=(+6H7X,   )                                                T0206344
      FMT(8)=(+6H4(F12.)                                                T0206346
      FMT(9)=(+6H3,8X),)                                                T0206348
      FMT(10)=(+6HF12.3))                                               T0206350
      TC=(+1H.)                                                         T0206353
      TP=(+1H+)                                                         T0206355
      CALL SCALE(WMIN,WMAX,100.0,JY,YMIN,YMAX,YIJ)                      T0206357
      YR=YMAX-YMIN                                                      T0206360
  230 J=JY                                                              T0206363
      IF(J*(J-10))204,201,201                                           T0206366
  201 IF(KL)220,220,231                                                 T0206370
 231  PRINT 1001                                                        T0206373
      IF(KL)250,250,220                                                 T0206376
  220 CLAB(1)= YMIN                                                     T0206380
      DO 222 I=2,11                                                     T0206383
  222 CLAB(I)=CLAB(I-1)+YIJ                                             T0206386
      PRINT 100,(CLAB(I),I=1,11,2),(CLAB(I),I=2,10,2)                   T0206390
      IF(KL)231,231,14                                                  T0206393
  204 IF(J-5)205,221,207                                                T0206396
  207 J=J-5                                                             T0206400
  205 JYT=5-J                                                           T0206403
  221 CONTINUE                                                          T0206406
      IF(KL)226,226,227                                                 T0206410
  226 FMT(3)=GF(JY)                                                     T0206413
  225 FMT(7)=GF(JY)                                                     T0206416
      TT=JY                                                             T0206420
      TT=TT*YIJ/10.                                                     T0206423
      CLAB(1)= YMIN+TT                                                  T0206426
      DO 223 I=2,10                                                     T0206430
  223 CLAB(I)=CLAB(I-1) +YIJ                                            T0206433
      PRINT FMT,(CLAB(I),I=2,10,2),(CLAB(I),I=1,9,2)                    T0206436
      IF(KL)227,227,14                                                  T0206440
  227 IF(JY-5)208,209,208                                               T0206443
 209  PRINT 1001                                                        T0206446
      IF(KL)250,250,226                                                 T0206450
 208  PRINT 1000,(TC,I=1,J),((TP,(TC,I=1,4)),K=1,19),TP,(               T0206453
     1 TC,I=1,JYT)                                                      T0206456
      IF(KL)250,250,226                                                 T0206460
  250 CONTINUE                                                          T0206463
      NCC=1                                                             T0206466
      IC=0                                                              T0206470
      IF(NP)80,11,11                                                    T0206473
   11 DO 1 I=1,51                                                       T0206476
       DO 1 J=1,17                                                      T0206480
   1  XY(I,J)=BLANKS                                                    T0206483
      CALL SCALE (ZMIN,ZMAX,50.,JX,XMIN,XMAX,XIJ)                       T0206486
      XR=XMAX-XMIN                                                      T0206490
   48 IF(NC)52,13,49                                                    T0206500
   49 IF(NP)80,10,10                                                    T0206510
   10 DO 9 N=1,NC                                                       T0206520
      SYMB=SYM(N)                                                       T0206530
      XDIFFR=XMAX-X                                                     T0206533
      IF(XDIFFR)105,106,106                                             T0206535
  105 XDIFFR=0.0                                                        T0206537
  106 YDIFFR=YMAX-Y(N)                                                  T0206540
      IF(YDIFFR)107,108,108                                             T0206543
  107 YDIFFR=0.0                                                        T0206545
  108 L=51.-(50.*XDIFFR)/XR+.5                                          T0206547
      K=101.-(100.*YDIFFR)/YR+.5                                        T0206550
      M=XMODF(K,6)                                                      T0206560
       K=(K-1)/6+1                                                      T0206570
      IF(M)21,16,21                                                     T0206580
   16 M=6                                                               T0206590
   21 LL=M                                                              T0206600
          M=(M-1)*6                                                     T0206610
 19   T=AND(XY(L,K),XM(LL))                                             T0206620
       CALL FORM2(T,M,SYMB)                                             T0206630
      XY(L,K)=OR(AND(XY(L,K),COMPL(XM(LL))),T)                          T0206640
 9     CONTINUE                                                         T0206650
      GO TO 15                                                          T0206660
   80 DO 86 I=1,17                                                      T0206670
   86 XY(1,I)=BLANKS                                                    T0206680
       L=1                                                              T0206690
      DO 95 N=1,NC                                                      T0206700
      SYMB=SYM(N)                                                       T0206710
      YDIFFR=YMAX-Y(N)                                                  T0206713
      IF(YDIFFR)860,865,865                                             T0206715
  860 YDIFFR=0.0                                                        T0206717
  865 K=101.-(100.*YDIFFR)/YR+.5                                        T0206720
      M=XMODF(K,6)                                                      T0206730
      IF(M)90,91,90                                                     T0206740
   91 M=6                                                               T0206750
   90 LL=M                                                              T0206760
       K=(K-1)/6+1                                                      T0206770
          M=(M-1)*6                                                     T0206780
      T=AND(XY(L,K),XM(LL))                                             T0206790
       CALL FORM2(T,M,SYMB)                                             T0206800
 95   XY(L,K)=OR(AND(XY(L,K),COMPL(XM(LL))),T)                          T0206810
      IF(XMODF(IC,5))97,96,97                                           T0206820
   96 W=TP                                                              T0206830
      GO TO 98                                                          T0206840
   97 W=TC                                                              T0206850
 98   PRINT 101,X,W,(XY(1,N),N=1,17),W                                  T0206860
      IC=IC+1                                                           T0206870
      GO TO 15                                                          T0206880
   13 M=6-JX                                                            T0206885
      LL=50+M                                                           T0206890
      T=JX                                                              T0206893
      IF(5-JX)131,131,135                                               T0206895
  131 T=0.0                                                             T0206897
  135 RLAB=XMAX-(T*XIJ)/5.0                                             T0206900
      W=TC                                                              T0206910
      K=52                                                              T0206915
      DO 31 L=M,LL                                                      T0206920
      K=K-1                                                             T0206925
      I=XMODF(L,5)                                                      T0206930
      IF(I-1)2,3,2                                                      T0206935
    3 W=TP                                                              T0206940
      PRINT 101,RLAB,W,(XY(K,N),N=1,17),W,RLAB                          T0206945
      RLAB=RLAB-XIJ                                                     T0206950
      W=TC                                                              T0206955
      GO TO 31                                                          T0206960
 2    PRINT 102,W,(XY(K,N),N=1,17),W                                    T0206965
   31 CONTINUE                                                          T0206970
   52 KL=1                                                              T0206980
      GO TO 230                                                         T0206990
   14 NCC=0                                                             T0207000
   15 RETURN                                                            T0207010
            END                                                         T0207020
C     LABEL                                                             T0207030
C     LIST8                                                             T0207040
C     FORTRAN                                                           T0207050
CPLUG         SUBROUTINE PLUG FOR BMD02T              JULY 22, 1964     T0207060
      SUBROUTINE PLUG (W)                                               T0207070
C                                                                       T0207080
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0207090
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0207100
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0207110
     3 CRO(400),COXYSQ(200),IPRNT1(20),IPRNT2(20)                       T0207120
      DIMENSION W(200),IPRNT3(20)                                       T0207130
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0207140
     -                              YP,SYM,CRO,COXYSQ,                  T0207150
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0207160
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0207170
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX          T0207180
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY) T0207190
C                                                                       T0207200
      SYM(1)=6H*00000                                                   T0207210
C                                                                       T0207220
      YMIN=9999999.0                                                    T0207230
      YMAX=-9999999.0                                                   T0207240
      DO 450 I=1,LLAG                                                   T0207250
      YMIN=MIN1F (YMIN,W(I))                                            T0207260
  450 YMAX=MAX1F (YMAX,W(I))                                            T0207270
      IF( LLAG-50) 455,460,460                                          T0207280
  455 KA=1                                                              T0207300
      KB=0                                                              T0207310
      GO TO 470                                                         T0207320
C                                                                       T0207330
 460  KA=-1                                                             T0207340
      KB=-1                                                             T0207350
 470  XMAX=0.5/DELTAT                                                   T0207360
 615  ANY=-PMD                                                          T0207370
      DO 630 I=1,LLAG                                                   T0207380
      ANY=ANY+PMD                                                       T0207390
      YP(1)=W(I)                                                        T0207400
  630 CALL PLOTR (ANY,0.00,XMAX,YP,SYM,YMIN,YMAX,1,KA)                  T0207410
      CALL PLOTR (ANY,0.00,XMAX,YP,SYM,YMIN,YMAX,KB,KA)                 T0207420
      RETURN                                                            T0207430
C                                                                       T0207440
      END                                                               T0207450
C     LABEL                                                             T0207460
C     LIST8                                                             T0207470
C     FORTRAN                                                           T0207480
CPOWER        SUBROUTINE POWER FOR BMD02T             JULY 22, 1964     T0207490
      SUBROUTINE POWER (NNX,X,XBAR,XALPHA,SPX)                          T0207500
C                                                                       T0207510
      AMDAF(FN,H,R)=((FN*H)**2)*(1.0-2.0*(R/FN)-2.0*(R/FN)**2)          T0207520
C                                                                       T0207530
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0207540
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0207550
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0207560
     3 CRO(400),COXYSQ(200),IPRNT1(20),IPRNT2(20)                       T0207570
      DIMENSION X(1000),SPX(200),IPRNT3(20)                             T0207580
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0207590
     -                              YP,SYM,CRO,COXYSQ,                  T0207600
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0207610
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0207620
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX,Q        T0207630
C                                                                       T0207640
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY),T0207650
     X(YES,IYES),(IA,A),(IB,B),(IP,P)                                   T0207660
C                                                                       T0207670
      A=6HA                                                             T0207680
      B=6HB                                                             T0207690
      BLANK=6H                                                          T0207700
      P=6HP                                                             T0207710
      STAR=6H*                                                          T0207720
      SYM(1)=6H*00000                                                   T0207730
      YES=6HYES                                                         T0207740
C                                                                       T0207750
      DO 301 I=1,LLAG                                                   T0207760
      SX=0.0                                                            T0207770
      M=I-1                                                             T0207780
      NX=NPOINT-M                                                       T0207790
      FNX=NX                                                            T0207800
      DO 305 J=1,NX                                                     T0207810
      K=M+J                                                             T0207820
  305 SX=SX+X(J)*X(K)                                                   T0207830
  301 RX(I)=SX/FNX                                                      T0207840
      IF(IDETRN-IYES)303,302,303                                        T0207850
 302  FI=-1.0                                                           T0207860
      XBAR2=XBAR*XBAR                                                   T0207870
      XALSQ=(XALPHA*XALPHA)/12.0                                        T0207880
      DO 306 I=1,LLAG                                                   T0207890
      FI=FI+1.0                                                         T0207900
      FLAMDA=AMDAF(FNPT,DELTAT,FI)                                      T0207910
 306  RX(I)=RX(I)-XBAR2-(FLAMDA*XALSQ)                                  T0207920
 303  PRINT 1301,UNIT,NNX                                               T0207930
 1301 FORMAT(1H1,21X,3HLAG,30X,14HAUTOCOVARIANCE/20X,1H(,A6,1H),28X,9HOFT0207940
     X SERIES,I3//)                                                     T0207950
      TIME=-DELTAT                                                      T0207960
      DO 320 I=1,LLAG                                                   T0207970
      TIME=TIME+DELTAT                                                  T0207980
 320  PRINT 1302,TIME,RX(I)                                             T0207990
 1302 FORMAT(19X,F9.4,26X,F13.6)                                        T0208000
      TIME1=TIME                                                        T0208010
      IF(IPRNT1(KIT+1)-IB)321,325,321                                   T0208020
 321  IF(IPRNT1(KIT+1)-IA)615,325,615                                   T0208030
  325 RXMIN=10**10                                                      T0208040
      RXMAX=-10**10                                                     T0208050
      PRINT 1303,NNX,TIME1,UNIT                                         T0208060
 1303 FORMAT(1H1,9X,42HGRAPH OF AUTOCOVARIANCE FUNCTION OF SERIES,I3,37HT0208070
     X PLOTTED AGAINST TIME UP TO A LAG OF F9.4,1X,A6//)                T0208080
      DO 335 I=1,LLAG                                                   T0208090
      RXMIN=MIN1F(RXMIN,RX(I))                                          T0208100
  335 RXMAX=MAX1F(RXMAX,RX(I))                                          T0208110
      ANY=-DELTAT                                                       T0208150
      IF(LLAG-50) 600,605,605                                           T0208160
 600  K1=0                                                              T0208170
      K2=1                                                              T0208180
      GO TO 606                                                         T0208190
C                                                                       T0208200
 605  K1=-1                                                             T0208210
      K2=K1                                                             T0208220
 606  DO 610 I=1,LLAG                                                   T0208230
      ANY=ANY+DELTAT                                                    T0208240
      YP(1)=RX(I)                                                       T0208250
  610 CALL PLOTR(ANY,0.,TIME1,YP,SYM,RXMIN,RXMAX,1,K2)                  T0208260
      CALL PLOTR(ANY,0.,TIME1,YP,SYM,RXMIN,RXMAX,K1,K2)                 T0208270
  615 SX=RX(1)                                                          T0208280
      RX(1)=0.5*RX(1)                                                   T0208290
      RX(LLAG)=0.5*RX(LLAG)                                             T0208300
      S1=-Q                                                             T0208310
      DO 401 IH=1,LLAG                                                  T0208320
      PX(IH)=0.0                                                        T0208330
      S1=S1+Q                                                           T0208340
      S=-S1                                                             T0208350
      DO 402 JP=1,LLAG                                                  T0208360
      S=S+S1                                                            T0208370
  402 PX(IH)=PX(IH)+RX(JP)*COSF(S)                                      T0208380
  401 PX(IH)=PX(IH)*CONST                                               T0208390
      SPX(1)=.54*PX(1)+.46*PX(2)                                        T0208400
      SPX(LLAG)=.54*PX(LLAG)+.46*PX(LLAG-1)                             T0208410
      KK=LLAG-1                                                         T0208420
      DO 415 J=2,KK                                                     T0208430
  415 SPX(J)=.54*PX(J)+.23*(PX(J+1)+PX(J-1))                            T0208440
      OMEGA=-Q                                                          T0208450
      THET1=1.0+(THETA*THETA)                                           T0208460
      THET2=2.0*THETA                                                   T0208470
      IF(IPREWT-IYES)420,425,420                                        T0208480
  425 DO 100 I=1,LLAG                                                   T0208490
      OMEGA=OMEGA+Q                                                     T0208500
      AA=THET1-THET2*COSF(OMEGA)                                        T0208510
  100 SPX(I)=SPX(I)/AA                                                  T0208520
 420  PRINT 1401,UNIT,NNX                                               T0208530
 1401 FORMAT(1H119X9HFREQUENCY22X24HPOWER SPECTRAL ESTIMATES/17X,8H(CYCLT0208540
     -ES/A6,1H),25X, 9HOF SERIES I3//)                                  T0208550
      ANY=-PMD                                                          T0208560
      ASSIGN 442 TO KADD1                                               T0208570
      ASSIGN 433 TO KADD2                                               T0208580
      RXMAX=-(0.5*(SPX(1)+SPX(LLAG)))                                   T0208590
      DO 435 I=1,LLAG                                                   T0208600
      ANY=ANY+PMD                                                       T0208610
      IF(SPX(I))421,422,422                                             T0208620
  421 RXY(I)=STAR                                                       T0208630
      GO TO 423                                                         T0208640
  422 RXY(I)=BLANK                                                      T0208650
 423  PRINT 1402,ANY,SPX(I),RXY(I)                                      T0208660
 1402 FORMAT(21X,F8.3,26X,F14.7,A1)                                     T0208670
      RXMAX=RXMAX+SPX(I)                                                T0208680
      IF(RXY(I)-BLANK)432,435,432                                       T0208690
  432 GO TO KADD2,(433,434)                                             T0208700
  433 SPMAX=-9999999.0                                                  T0208710
      DO 4335 J=1,LLAG                                                  T0208720
      SPMAX=MAX1F(SPMAX,SPX(J))                                         T0208730
 4335 CONTINUE                                                          T0208740
      ASSIGN 440 TO KADD1                                               T0208750
      ASSIGN 434 TO KADD2                                               T0208760
  434 SPX(I)=SPMAX                                                      T0208770
  435 CONTINUE                                                          T0208780
      GO TO KADD1,(440,442)                                             T0208790
 440  PRINT 1405                                                        T0208800
 1405 FORMAT(1H0, 6X,88H* THIS ESTIMATE IS NEGATIVE INDICATING SOME LEAKT0208810
     XAGE. IT WILL BE SET EQUAL TO THE MAXIMUM/ 9X,85HVALUE OF THE REMAIT0208820
     XNING ESTIMATES FOR FUTURE PLOTS AND EQUAL TO ZERO FOR CALCULATIONST0208830
     X.)                                                                T0208840
  442 SPMAX=(Q*RXMAX)/DELTAT                                            T0208850
      ANY=SPMAX-SX                                                      T0208860
      PRINT 1406,SPMAX,SX,ANY                                           T0208870
 1406 FORMAT(1H0,6X,44HTHE CHECK SUM OF POWER SPECTRAL ESTIMATES IS,F14.T0208880
     X7,14H AND SHOULD BE,F14.7/7X,17HTHE DIFFERENCE IS,F14.7)          T0208890
      IF(IPRNT1(KIT+1)-IB)443,448,443                                   T0208900
 443  IF(IPRNT1(KIT+1)-IP)476,448,476                                   T0208910
 448  IF(IPLOT) 450,460,450                                             T0208920
 450  PRINT 1404,NNX,UNIT                                               T0208930
 1404 FORMAT (1H1,18X,47HGRAPH OF THE POWER SPECTRAL ESTIMATES OF SERIEST0208940
     XI3,27H AGAINST FREQUENCY (CYCLES/A6,1H)//)                        T0208950
      CALL PLUG (SPX)                                                   T0208960
      IF(IPLOT) 476,460,460                                             T0208970
 460  PRINT 1403,NNX,UNIT                                               T0208980
 1403 FORMAT(1H1,10X,34HPOWER SPECTRAL ESTIMATES OF SERIESI3,6X49HPLOTTET0208990
     XD IN A LOG SCALE AGAINST FREQUENCY (CYCLES/A6,1H)//)              T0209000
      DO 475 I=1,LLAG                                                   T0209010
  475 RX(I)=LOGF(SPX(I))                                                T0209020
      CALL PLUG (RX)                                                    T0209030
  476 DO 471 I=1,LLAG                                                   T0209040
      IF(RXY(I)-BLANK)472,471,472                                       T0209050
  472 SPX(I)=0.0                                                        T0209060
  471 CONTINUE                                                          T0209070
  470 RETURN                                                            T0209080
C                                                                       T0209090
      END                                                               T0209100
      SUBROUTINE REMOVE(I)                                              T0209110
      ENDFILE I                                                         T0209120
      REWIND I                                                          T0209130
      RETURN                                                            T0209138
      END                                                               T0209140
      REAL FUNCTION   ATANF(A)                                          T0209150
      ATANF=ATAN(A)                                                     T0209160
      RETURN                                                            T0209170
      END                                                               T0209180
      REAL FUNCTION   LOGF(A)                                           T0209190
      LOGF=ALOG(A)                                                      T0209200
      RETURN                                                            T0209210
      END                                                               T0209220
      REAL FUNCTION   SQRTF(A)                                          T0209230
      SQRTF=SQRT(A)                                                     T0209240
      RETURN                                                            T0209250
      END                                                               T0209260
                                                                        T0209270
                                                                        T0209280
C     LABEL                                                             T0209290
C     LIST8                                                             T0209300
C     FORTRAN                                                           T0209310
CSMEAN   SUBROUTINE SMEAN FOR BMD02T                 MARCH 16, 1965     T0209320
      SUBROUTINE SMEAN (NNX,X,XBAR,XALPHA)                              T0209330
C                                                                       T0209340
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0209350
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0209360
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0209370
     3 CRO(400),COXYSQ(200),IPRNT1(20),IPRNT2(20)                       T0209380
      DIMENSION X(1000),IPRNT3(20)                                      T0209390
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0209400
     -                              YP,SYM,CRO,COXYSQ,                  T0209410
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0209420
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0209430
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX          T0209440
     -,Q,KPOINT                                                         T0209445
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY),T0209450
     X(YES,IYES)                                                        T0209460
C                                                                       T0209470
      YES=6HYES                                                         T0209480
      SYM(1)=6H*00000                                                   T0209490
C                                                                       T0209500
      NPOINT=KPOINT                                                     T0209505
      IF (INFORM) 100,105,105                                           T0209510
  100  DO 110 I=1,NPOINT                                                T0209520
      ND=I+(NNX-1)*NPOINT                                               T0209530
  110  X(I)=Z(ND)                                                       T0209540
      GO TO 135                                                         T0209550
  105 ISX=NPOINT*(NNX-1)                                                T0209560
      DO 115 I=1,NPOINT                                                 T0209570
      K=I+ISX                                                           T0209580
  115 X(I)=Z(K)                                                         T0209590
 135  IF(IORDAT-IYES)140,130,140                                        T0209600
 130  PRINT 1102,NNX                                                    T0209610
 1102 FORMAT(1H15X24HORIGINAL DATA OF SERIES I3//)                      T0209620
      PRINT 1104,(X(I),I=1,NPOINT)                                      T0209630
 1104 FORMAT(10F11.5)                                                   T0209640
 140  IF(IPLDAT-IYES)141,200,141                                        T0209650
  200 HU=-10**10                                                        T0209660
      SM=10**10                                                         T0209670
      DO 210 I=1,NPOINT                                                 T0209680
      HU=MAX1F(HU,X(I))                                                 T0209690
  210 SM=MIN1F (SM,X(I))                                                T0209700
      PRINT 2001,NNX                                                    T0209740
 2001 FORMAT(1H1,10X,22HGRAPH OF INPUT SERIES I3///)                    T0209750
      FI=0.0                                                            T0209760
      DO 215 I=1,NPOINT                                                 T0209770
      FI=FI+1.0                                                         T0209780
      YP(1)=X(I)                                                        T0209790
      CALL PLOTR (FI,1.0,FNPT,YP,SYM,SM,HU,1,-1)                        T0209800
  215 CONTINUE                                                          T0209810
      CALL PLOTR (FI,1.0,FNPT,YP,SYM,SM,HU,-1,-1)                       T0209820
  141 MISTAK=0                                                          T0209830
  150 LL=KIT+1                                                          T0209840
      IF(NNX-LISA(LL)) 145,155,145                                      T0209850
  155 CALL TRANS(NNX,X)                                                 T0209860
      IF (MISTAK) 160,145,160                                           T0209870
 145  IF(IDETRN-IYES)305,300,305                                        T0209880
  300 PLUI=0.0                                                          T0209890
      MU=(NPOINT+2)/3                                                   T0209900
      NSMALL =NPOINT-MU+1                                               T0209910
      DO 310 I=NSMALL,NPOINT                                            T0209920
  310 PLUI=PLUI+X(I)                                                    T0209930
      PHO=0.0                                                           T0209940
      DO 315 I=1,MU                                                     T0209950
  315 PHO=PHO+X(I)                                                      T0209960
      FMU=MU                                                            T0209970
      XALPHA=(PLUI-PHO)/(DELTAT*FMU*(FNPT-FMU))                         T0209980
  305 SUMX=0.0                                                          T0209990
      DO 320 I=1,NPOINT                                                 T0210000
  320 SUMX=SUMX+X(I)                                                    T0210010
      XBAR=SUMX/FNPT                                                    T0210020
      IF(IDETRN-IYES)325,330,325                                        T0210030
  325 DO 335 I=1,NPOINT                                                 T0210040
  335 X(I)=X(I)-XBAR                                                    T0210050
 330  IF(IPREWT-IYES)160,340,160                                        T0210060
  340 NT=NPOINT-1                                                       T0210070
      DO 350 I=1,NT                                                     T0210080
  350 X(I)=X(I+1)-THETA*X(I)                                            T0210090
      NPOINT=NT                                                         T0210100
      FNPT=NPOINT                                                       T0210110
  160 RETURN                                                            T0210120
C                                                                       T0210130
      END                                                               T0210140
C     LABEL                                                             T0210150
C     LIST8                                                             T0210160
C     FORTRAN                                                           T0210170
CTPWD         SUBROUTINE TPWD FOR BMDXXX SERIES                         T0210180
      SUBROUTINE TPWD(NT1,NT2)                                          T0210190
      IF(NT1)40,10,12                                                   T0210200
 10   NT1=5                                                             T0210210
 12   IF(NT1-NT2)14,19,14                                               T0210220
 14   IF(NT2-5)15,19,17                                                 T0210230
   15 REWIND NT2                                                        T0210240
      GO TO 19                                                          T0210250
   17 CALL REMOVE(NT2)                                                  T0210260
   19 IF(NT1-5)18,24,18                                                 T0210270
 18   IF(NT1-6)22,40,22                                                 T0210280
 22   REWIND NT1                                                        T0210290
 24   NT2=NT1                                                           T0210300
 28   RETURN                                                            T0210310
 40   PRINT 49                                                          T0210320
      CALL EXIT                                                         T0210330
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              T0210340
      END                                                               T0210350
C     LABEL                                                             T0210360
C     LIST8                                                             T0210370
C     FORTRAN                                                           T0210380
CTRANS        SUBROUTINE TRANS FOR BMD02T             JUNE 2, 1964      T0210390
      SUBROUTINE TRANS(NNX,X)                                           T0210400
C                                                                       T0210410
      ASNF(V)=ATANF(V/SQRTF(1.0-V**2))                                  T0210420
C                                                                       T0210430
      DIMENSION Z(17000),DUMB(20),FMT(120),NSELL(20),RX(200),PX(200),   T0210440
     1 LISA(20),ADDXY(200),SUBXY(200),CXY(200),QXY(200),SCXY(200),      T0210450
     2 SQXY(200),AMXY(200),PHASXY(200),RXY(200),RYX(200),YP( 4),SYM( 4),T0210460
     3 CRO(400),COXYSQ(200),CON( 8),IBIN( 8),IPRNT1(20),IPRNT2(20)      T0210470
      DIMENSION X(1000),IPRNT3(20)                                      T0210480
      COMMON Z,DUMB,FMT,NSELL,RX,PX,LISA,ADDXY,SUBXY,CXY,QXY,SCXY,SQXY, T0210490
     -                              YP,SYM,CRO,COXYSQ                   T0210500
     - IORDAT,IPLDAT,IDETRN,IPREWT,THETA,KSER,NPOINT,LAG,NSEL,IPLOT,    T0210510
     - DELTAT,UNIT,INFORM,KVR,IPRNT1,IPRNT2,IPRNT3,IPOW,ICCS,ICOH,      T0210520
     - PI,FNPT,CONST,PMD,FLAG,FLLAG,LLAG,KIT,KDUM,MISTAK,MX,KX          T0210530
      EQUIVALENCE (RXY,CXY,AMXY),(RYX,QXY,PHASXY),(RX,ADDXY),(PX,SUBXY),T0210540
     X(SPC,ISPC)                                                        T0210550
C                                                                       T0210560
      SPC   =(+6HSPECTG)                                                T0210570
C                                                                       T0210580
      READ 1002,JOB,NTRAN,(IBIN(I),CON(I),I=1,NTRAN)                    T0210590
 1002 FORMAT(A6,I1,8(I2,F6.0))                                          T0210600
      IF(JOB-ISPC)700,602,700                                           T0210610
 602  DO 500 I=1,NTRAN                                                  T0210620
      IF(IBIN(I)-17) 605,600,605                                        T0210630
  605 JESUS=IBIN(I)                                                     T0210640
      IF(JESUS-6)610,905,610                                            T0210650
C                                                                       T0210660
  600 JESUS=6                                                           T0210670
  610 CC=CON(I)                                                         T0210680
      IF(JESUS*(JESUS-11)) 620,905,905                                  T0210685
  620 DO 150 K=1,NPOINT                                                 T0210687
      GO TO (10,20,30,40,50,60,70,80,90,100) ,JESUS                     T0210690
   10 IF(X(K))200,150,14                                                T0210710
C                                                                       T0210740
   14 X(K)=SQRTF(X(K))                                                  T0210750
      GO TO 150                                                         T0210770
C                                                                       T0210780
   20 IF(X(K))200,22,23                                                 T0210800
   22 X(K)=1.0                                                          T0210810
      GO TO 150                                                         T0210820
C                                                                       T0210830
   23  X(K)=SQRTF(X(K))+SQRTF(X(K)+1.0)                                 T0210840
      GO TO 150                                                         T0210850
   30 IF(X(K))200,200,31                                                T0210870
   31 X(K)=0.4342944819*LOGF(X(K))                                      T0210880
      GO TO 150                                                         T0210890
C                                                                       T0210900
   40 X(K)=EXPF(X(K))                                                   T0210920
      GO TO 150                                                         T0210930
C                                                                       T0210940
   50 IF(X(K))200,150,53                                                T0210960
C                                                                       T0210990
   53 IF (X(K)-1.0) 54,55,200                                           T0211000
  54  ARG =SQRTF(X(K))                                                  T0211010
      X(K)=ASNF(ARG)                                                    T0211020
      GO TO 150                                                         T0211030
C                                                                       T0211040
   55 X(K)=PI/2.0                                                       T0211050
      GO TO 150                                                         T0211070
C                                                                       T0211080
   60 IF(X(K))200,200,61                                                T0211100
   61 X(K)=LOGF(X(K))                                                   T0211110
      GO TO 150                                                         T0211120
C                                                                       T0211130
   70 IF(X(K))71,200,71                                                 T0211150
   71 X(K)=1.0/X(K)                                                     T0211160
      GO TO 150                                                         T0211170
C                                                                       T0211180
   80 X(K)=X(K)+CC                                                      T0211200
      GO TO 150                                                         T0211210
C                                                                       T0211220
   90 X(K)=X(K)*CC                                                      T0211240
      GO TO 150                                                         T0211250
C                                                                       T0211260
  100 IF(X(K))200,200,101                                               T0211280
  101 X(K)=X(K)**CC                                                     T0211290
      GO TO 150                                                         T0211295
 200  PRINT 1001,K,NNX,IBIN(I)                                          T0211300
 1001 FORMAT(11H0DATA POINT I5,10H OF SERIESI3,                         T0211310
     X57H VIOLATES THE RESTRICTION FOR TRANSGENERATION OF THE TYPEI3,   T0211315
     X52H. THE PROGRAM CONTINUES LEAVING THE VALUE UNCHANGED.)          T0211320
  150 CONTINUE                                                          T0211330
  500 CONTINUE                                                          T0211340
  300 RETURN                                                            T0211350
 700  PRINT 1003,JOB                                                    T0211360
 901  PRINT 1004                                                        T0211370
      MISTAK=11                                                         T0211380
      GO TO 300                                                         T0211390
 1003 FORMAT(58H0CONTROL CARD ERROR. PROGRAM EXPECTED A SPECTG CARD BUT T0211400
     XA A6,16H CARD WAS FOUND.)                                         T0211405
 1004 FORMAT(55H0PROGRAM WILL GO TO THE NEXT SELECTION OR PROBLEM CARD.)T0211410
 905  PRINT 1005                                                        T0211415
      GO TO 901                                                         T0211420
 1005 FORMAT(40H0ILLEGAL TRANSGENERATION CODE SPECIFIED.)               T0211425
      END                                                               T0211430
C     LABEL                                                             T0211440
C     LIST8                                                             T0211450
C     FORTRAN                                                           T0211460
CVFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDST0211470
      SUBROUTINE VFCHCK(NVF)                                            T0211480
      IF(NVF)10,10,20                                                   T0211490
 10   PRINT 4000                                                        T0211500
      NVF=1                                                             T0211510
 50   RETURN                                                            T0211520
C                                                                       T0211530
 20   IF(NVF-10)50,50,10                                                T0211540
C                                                                       T0211550
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFT0211560
     XIED, ASSUMED TO BE 1.)                                            T0211570
      END                                                               T0211580
C     LABEL                                                             T0211590
C     LIST8                                                             T0211600
C     FORTRAN                                                           T0211610
CSCALE        SUBROUTINE SCALE FOR SUB PLOTR        AUGUST 18, 1964     T0211620
      SUBROUTINE SCALE(YMIN,YMAX,YINT,JY,TYMIN,TYMAX,YIJ)               T0211630
      DIMENSION C(10)                                                   T0211640
       C(1)=  1.0                                                       T0211650
       C(2)=1.5                                                         T0211660
       C(3)=2.0                                                         T0211670
       C(4)=3.0                                                         T0211680
       C(5)=4.0                                                         T0211690
      C(6)=5.0                                                          T0211700
       C(7)=7.5                                                         T0211710
       C(8)=10.0                                                        T0211720
      TEST=2**(-21)                                                     T0211730
   50 YR=YMAX-YMIN                                                      T0211740
      TT=YR/YINT                                                        T0211750
      J=LOG10F(TT)                                                      T0211760
      E=10.0**J                                                         T0211770
      TT=TT/E                                                           T0211780
      I=0                                                               T0211790
      IF(TT-1.0)205,201,201                                             T0211800
  205 TT=TT*10.0                                                        T0211810
      E=E/10.0                                                          T0211820
 201  I=I+1                                                             T0211830
      IF(8-I)1,2,2                                                      T0211840
    1 E=E*10.0                                                          T0211850
      I=1                                                               T0211860
  2   T3=TT-C(I)                                                        T0211870
      IF (T3.GT. .000001 .OR. T3 .LT. -.000001) GO TO 252               T0211872
      TT=INT(TT*1000000)/1000000                                        T0211874
 252  IF (TT-C(I))233,202,201                                           T0211876
  233 YIJ=C(I)*E                                                        T0211880
      GO TO 203                                                         T0211890
  202 Y=YMIN/C(I)                                                       T0211900
      J=Y                                                               T0211910
      T=J                                                               T0211920
      GO TO 233                                                         T0211930
  204 YIJ=C(I+1)*E                                                      T0211940
  203 X=((YMAX+YMIN)/YIJ-YINT )/2.0+.00001                              T0211950
      K=X                                                               T0211960
      IF(K)235,240,240                                                  T0211970
  235 Y=K                                                               T0211980
      IF(X-Y)236,240,236                                                T0211990
  236 K=K-1                                                             T0212000
  240 TYMIN=K                                                           T0212010
      TYMIN=YIJ*TYMIN                                                   T0212020
      TYMAX=TYMIN+YINT*YIJ                                              T0212030
      IF(YMAX-TYMAX-TEST)10,10,201                                      T0212040
   10 TT=YINT/10.                                                       T0212050
      JY=TT+.000001                                                     T0212060
      YIJ=YINT*(YIJ/10.0)                                               T0212070
      J=TYMIN/ YIJ                                                      T0212080
      IF (K)242,241,241                                                 T0212090
  242 J=J-1                                                             T0212100
  241 J=J*JY+JY-K                                                       T0212110
      JY=J                                                              T0212120
      RETURN                                                            T0212130
      END                                                               T0212140
      REAL FUNCTION   SINF(A)                                           T0212150
      SINF=SIN(A)                                                       T0212160
      RETURN                                                            T0212170
      END                                                               T0212180
      REAL FUNCTION   COSF(A)                                           T0212190
      COSF=COS(A)                                                       T0212200
      RETURN                                                            T0212210
      END                                                               T0212220
      REAL FUNCTION   MIN1F(A,B)                                        T0212230
      MIN1F=AMIN1(A,B)                                                  T0212240
      RETURN                                                            T0212250
      END                                                               T0212260
      REAL FUNCTION   MAX1F(A,B)                                        T0212270
      MAX1F=AMAX1(A,B)                                                  T0212280
      RETURN                                                            T0212290
      END                                                               T0212300
      REAL FUNCTION   ABSF(A)                                           T0212310
      ABSF=ABS(A)                                                       T0212320
      RETURN                                                            T0212330
      END                                                               T0212340
