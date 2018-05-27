         BEGIN                                                          MAIN0010
COMMENT       THIS TEST PROGRAM USES PROCEDURE ANALYTIC TO EVALUATE     MAIN0020
              THE ANALYTIC CONTINUATION OF THE HYPERGEOMETRIC SERIES    MAIN0030
              IN ANY OF SIX DIFFERENT FORMS .                           MAIN0040
                                                                        MAIN0050
              ROBERT R. HAMMERS ,                                       MAIN0060
              BURROUGHS CORPORATION ,                                   MAIN0070
              (PROFESSIONAL SERVICES) ,                                 MAIN0080
              PASADENA , CALIFORNIA ,                                   MAIN0090
                                                                        MAIN0100
                                                                        MAIN0110
              CARD SEQUENCE FOR MAIN PROGRAM BEGINS WITH MAIN0010 .     MAIN0120
              CARD SEQUENCE FOR PROCEDURE ANALYTIC BEGINS WITH PROC0010.MAIN0130
                                                                        MAIN0140
              FIRST RELEASE 10 - 31 - 63  .  ;                          MAIN0150
                                                                        MAIN0160
                                                                        MAIN0170
FILE IN       FIN (1,10)  ;                                             MAIN0180
                                                                        MAIN0190
FILE OUT      FLO (1,15)  ;                                             MAIN0200
                                                                        MAIN0210
REAL          AI,BI,CI,ZR,ZI,EPS,FR,FI  ;                               MAIN0220
INTEGER       CMAX,TIN  ;                                               MAIN0230
LABEL         START,ERROR,IERR,ZERODEN,FINISH,EXCMAX   ;                MAIN0240
LIST          LIST1 (AI,BI,CI,ZR,ZI,EPS,CMAX,TIN)  ;                    MAIN0250
                                                                        MAIN0260
FORMAT IN     FREAD (5F12.6,F12.9,2I4)  ;                               MAIN0270
                                                                        MAIN0280
FORMAT OUT    FERR ("AN ARGUMENT FOR THE GAMMA FUNCTION IS 0 OR A NEG", MAIN0290
                    "ATIVE INTEGER"//)  ,                               MAIN0300
              FCMAX ("HYPERGEOMETRIC SERIES CONVERGING TOO SLOWLY, "    MAIN0310
              " CMAX IS EXCEEDED"//)  ,                                 MAIN0320
              FARG ("ARGUMENT OUT OF RANGE"//)  ,                       MAIN0330
              FANS ("REAL PART OF ANSWER =",F20.12/                     MAIN0340
                    "IMAGINARY PART OF ANSWER =",F20.12//)  ,           MAIN0350
              OPENFILE ("AI =",E20.12," BI =",E20.12," CI =",E20.12,X1, MAIN0360
              "ZR =",E20.12/"ZI =",E20.12," EPS ",E20.12," CMAX =",I4,  MAIN0370
              " TIN =",I4//)  ,                                         MAIN0380
              FBOG ("A DENOMINATOR TERM IS 0 IN THE HYPERGEOMETRIC SER",MAIN0390
                    "IES"//)  ;                                         MAIN0400
                                                                        MAIN0410
PROCEDURE     ANALYTIC  (AI,BI,CI,ZR,ZI,EPS,CMAX,TIN,FR,FI)  ;          PROC0010
                                                                        PROC0020
VALUE         AI,BI,CI,ZR,ZI,EPS,CMAX,TIN  ;                            PROC0030
REAL          AI,BI,CI,ZR,ZI,EPS,FR,FI  ;                               PROC0040
INTEGER       CMAX,TIN  ;                                               PROC0050
                                                                        PROC0060
         BEGIN                                                          PROC0070
                                                                        PROC0080
COMMENT       AI,BI,AND CI  - THE NUMERATOR AND DENOMINATOR PARAMETERS  PROC0090
                               FOR THE HYPERGEOMETRIC SERIES.           PROC0100
              ZR AND ZI - THE REAL AND COMPLEX PARTS OF THE ARGUEMENT.  PROC0110
              EPS  - THE RELATIVE ERROR TOLERANCE USED IN PROCEDURE     PROC0120
                     HYPER1 .                                           PROC0130
              CMAX - THE MAXIMUM NUMBER OF TERMS ALLOWED IN THE SERIES  PROC0140
                     EVALUATION .                                       PROC0150
              TIN  - INTEGER WITH WHICH PROGRAMMER CAN CHOOSE DESIRED   PROC0160
                     FORM .                                             PROC0170
              FR,FI - REAL AND IMAGINARY EVALUATION OF ANALYTIC         PROC0180
                      CONTINUATION (E.G. OUTPUT PARAMETERS) .           PROC0190
                   THREE EXTERNAL LABELS MUST BE PROVIDED BY PROGRAMMER PROC0200
                   WHEN USING PROCEDURE ANALYTIC TO WHICH CONTROL IS    PROC0210
                   SWITCHED UNDER THE FOLLOWIMG CONDITIONS:             PROC0220
              ERROR: ARGUEMENT FOR GAMMA FUNCTION IS ZERO OR A NEGATIVE PROC0230
                     INTEGER . GAMMA = INFINITY .                       PROC0240
              IERR : ARGUEMENT (ZR OR ZI) OUT OF RANGE .                PROC0250
              ZERODEN: HYPERGEOMETRIC SERIES DIVERGES TO INFINITY       PROC0260
                       BECAUSE A DENOMINATOR TERM GOES TO ZERO .  ;     PROC0270
                                                                        PROC0280
REAL          C1,C2,C3,C4,C5,C6,A,A1,A2,A3,B,B1,B2,B3,AG1,AG2,ALR,ALI,  PROC0290
              ANYR,ANYI,PR1,PR2,PU1,PU2,MIN,TMP,G1,G2,G3,G4,G5,G6,G7,G8,PROC0300
              G9  ;                                                     PROC0310
INTEGER       I  ;                                                      PROC0320
ARRAY         BAR[0:5]  ;                                               PROC0330
LABEL         F1,F2,F3,F4,F5,F6,TEST,GCALC,L1,L2,RETURN,F3A  ;          PROC0340
                                                                        PROC0350
REAL PROCEDURE GAMMA(X)  ;                                              PROC0360
                                                                        PROC0370
VALUE  X  ;  REAL X  ;                                                  PROC0380
                                                                        PROC0390
         BEGIN                                                          PROC0400
                                                                        PROC0410
INTEGER       I  ;                                                      PROC0420
REAL          H,Y,S;                                                    PROC0430
LABEL         A1,A2  ;                                                  PROC0440
                                                                        PROC0450
              H ~ 1.  ;  Y ~ X  ;                                       PROC0460
A1:           IF Y=0 THEN H ~ 1@36 ELSE                                 PROC0470
              IF Y=2. THEN GO TO A2 ELSE                                PROC0480
              IF Y<2. THEN                                              PROC0490
         BEGIN                                                          PROC0500
              H ~ H/Y  ;  Y ~ Y+1.  ;  GO TO A1                         PROC0510
         END  ELSE                                                      PROC0520
              IF Y } 3. THEN                                            PROC0530
         BEGIN                                                          PROC0540
              Y ~ Y-1.  ;  H ~ H|Y  ;  GO TO A1                         PROC0550
         END  ELSE                                                      PROC0560
         BEGIN                                                          PROC0570
              Y ~ Y - 2  ;                                              PROC0580
              S ~ ((((((((( .6771057117@-4|Y - .34423420456@-3 )|Y +    PROC0590
              .00153976810472)|Y - .00246674798054)|Y + .0109736958417) PROC0600
              |Y - .21090746731@-3)|Y + .0742379076063)|Y +             PROC0610
              .0815782187849)|Y + .411840251796)|Y + .422784336962)|Y + PROC0620
              .999999999993  ;  H ~ S|H                                 PROC0630
         END  ;                                                         PROC0640
                                                                        PROC0650
A2:           GAMMA ~ H                                                 PROC0660
                                                                        PROC0670
         END  ;                                                         PROC0680
                                                                        PROC0690
PROCEDURE     HYPER1 (A,B,C,X,Y,CMAX,EPS,SUM,ISUM)  ;                   PROC0700
                                                                        PROC0710
VALUE         A,B,C,X,Y,CMAX,EPS   ;                                    PROC0720
REAL          A,B,C,X,Y,EPS,SUM,ISUM  ;                                 PROC0730
INTEGER       CMAX  ;                                                   PROC0740
                                                                        PROC0750
         BEGIN                                                          PROC0760
                                                                        PROC0770
REAL          HYP,HYI,ATERM,GTERM,TEM1,TEM2  ;                          PROC0780
INTEGER       COUNT,LEASTMIN  ;                                         PROC0790
LABEL         RECURSE,L,TESTI  ;                                        PROC0800
                                                                        PROC0810
              IF SQRT (X*2 + Y*2) } 1.0 THEN GO TO IERR  ;              PROC0820
              HYP ~ SUM ~ 1.  ;  HYI ~ ISUM ~ COUNT ~ 0  ;              PROC0830
              LEASTMIN ~ IF A < B THEN A ELSE B  ;                      PROC0840
              LEASTMIN ~ IF C < LEASTMIN THEN C ELSE LEASTMIN  ;        PROC0850
              LEASTMIN ~ IF LEASTMIN < 0 THEN -LEASTMIN+2 ELSE 0  ;     PROC0860
                                                                        PROC0870
RECURSE:      ATERM ~ (A+COUNT)|(B+COUNT)  ;  IF ATERM = 0 THEN GO TO L;PROC0880
              GTERM ~  C+COUNT   ;  IF GTERM = 0 THEN                   PROC0890
              GO TO ZERODEN  ;                                          PROC0900
              COUNT ~ COUNT+1  ;  IF Y ! 0 THEN                         PROC0910
         BEGIN                                                          PROC0920
              TEM1 ~ ATERM/(GTERM|COUNT)  ;                             PROC0930
              TEM2 ~ (HYP|X-Y|HYI)|TEM1   ;                             PROC0940
              HYI  ~ (HYP|Y+X|HYI)|TEM1   ;   HYP ~ TEM2         ;      PROC0950
              SUM  ~  HYP  +  SUM         ;  ISUM ~ ISUM + HYI   ;      PROC0960
              IF COUNT < LEASTMIN THEN GO TO RECURSE  ;                 PROC0970
              IF SUM = 0 THEN                                           PROC0980
         BEGIN                                                          PROC0990
              IF ISUM ! 0 THEN                                          PROC1000
         BEGIN                                                          PROC1010
                                                                        PROC1020
TESTI:        IF ABS( HYI /ISUM) < EPS THEN GO TO L                     PROC1030
         END  ;                                                         PROC1040
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCMAX       PROC1050
         END  ;                                                         PROC1060
              IF ABS( HYP /SUM) < EPS THEN                              PROC1070
         BEGIN                                                          PROC1080
              IF ISUM = 0 THEN GO TO L  ;  GO TO TESTI                  PROC1090
         END  ;                                                         PROC1100
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCMAX  ;    PROC1110
         END  ;                                                         PROC1120
              HYP ~ ATERM/(GTERM|COUNT)|X|HYP  ;                        PROC1130
              SUM ~ SUM + HYP  ;                                        PROC1140
              IF SUM = 0 THEN                                           PROC1150
         BEGIN                                                          PROC1160
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCMAX       PROC1170
         END  ;                                                         PROC1180
              IF ABS(HYP/SUM) < EPS THEN GO TO L  ;                     PROC1190
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCMAX  ;    PROC1200
                                                                        PROC1210
L:       END  ;                                                         PROC1220
                                                                        PROC1230
PROCEDURE     POWC (X,Y,W,A,B)  ;                                       PROC1240
                                                                        PROC1250
VALUE         X,Y,W  ;                                                  PROC1260
REAL          X,Y,W,A,B  ;                                              PROC1270
                                                                        PROC1280
         BEGIN                                                          PROC1290
                                                                        PROC1300
REAL          THETA,PHI,R  ;                                            PROC1310
LABEL         SOL1,SOL2,RETURN  ;                                       PROC1320
                                                                        PROC1330
              IF X>0 THEN                                               PROC1340
         BEGIN                                                          PROC1350
              THETA ~ 0  ;  GO TO SOL1                                  PROC1360
         END  ;                                                         PROC1370
              IF X=0 THEN                                               PROC1380
         BEGIN                                                          PROC1390
              IF Y=0 THEN                                               PROC1400
         BEGIN                                                          PROC1410
              A ~ B ~ 0  ;  GO TO RETURN                                PROC1420
         END  ;                                                         PROC1430
              PHI ~ SIGN(Y)| 1.5707963268  ;  GO TO SOL2                PROC1440
         END  ;                                                         PROC1450
              THETA ~ SIGN(Y)|3.1415926536  ;  IF Y  = 0 THEN THETA ~   PROC1460
              3.1415926536  ;                                           PROC1470
SOL1:         PHI ~ ARCTAN(Y/X)+THETA  ;                                PROC1480
SOL2:         R ~ EXP(W|LN(SQRT(X*2+Y*2)))  ;                           PROC1490
              A ~ R|COS(W|PHI)  ;  B ~ R|SIN(W|PHI)  ;                  PROC1500
                                                                        PROC1510
RETURN:  END  ;                                                         PROC1520
                                                                        PROC1530
              FR ~ FI ~ 0  ;                                            PROC1540
              C1 ~ SQRT ((1.-ZR)*2+ZI*2)  ;                             PROC1550
              C6 ~ SQRT (ZR*2+ZI*2)       ;                             PROC1560
              C2 ~ 1/C6  ;  C3 ~ 1/C1     ;                             PROC1570
              C4 ~ C5 ~ 1/ABS(ZR-.5)      ;                             PROC1580
              IF (TIN } 1) AND (TIN { 4) THEN GO TO TEST  ;             PROC1590
              IF TIN = 5 THEN GO TO F5  ;  IF TIN = 6 THEN GO TO F6  ;  PROC1600
              IF (MIN ~ IF C1 < C2 THEN C1 ELSE C2) > C3 THEN MIN ~ C3 ;PROC1610
              IF MIN > C4 THEN MIN ~ C4  ;                              PROC1620
              IF MIN > C5 THEN MIN ~ C5  ;                              PROC1630
              IF MIN > C6 THEN MIN ~ C6  ;                              PROC1640
              IF MIN = C6 THEN GO TO F6  ;                              PROC1650
              IF MIN = C5 THEN GO TO F5  ;                              PROC1660
TEST:         BAR[1] ~ AI  ;  BAR[2] ~ BI  ;  BAR[3] ~ CI  ;            PROC1670
              BAR[4] ~ CI-AI  ;  BAR[5] ~ CI-BI  ;                      PROC1680
                                                                        PROC1690
              FOR I ~ 1,2,3,4,5 DO IF TMP ~ BAR[I] = ENTIER(TMP) AND    PROC1700
                                      TMP { 0 THEN GO TO ERROR  ;       PROC1710
              IF (TIN = 1) OR (TIN = 4) THEN GO TO L1  ;                PROC1720
              IF (TIN = 2) OR (TIN = 3) THEN GO TO L2  ;                PROC1730
              IF MIN = C1 OR MIN = C4 THEN                              PROC1740
L1:           IF G9 ~ CI-AI-BI = ENTIER(G9) THEN GO TO ERROR ELSE       PROC1750
         BEGIN                                                          PROC1760
              G8 ~ GAMMA(G9)  ;  G9 ~ GAMMA(-G9)  ;  GO TO GCALC        PROC1770
         END  ;                                                         PROC1780
L2:           IF G5 ~ AI-BI = ENTIER(G5) THEN GO TO ERROR  ;            PROC1790
              G4 ~ GAMMA(G5)  ;  G5 ~ GAMMA(-G5)  ;                     PROC1800
GCALC:        G1 ~ GAMMA(AI)  ;  G2 ~ GAMMA(BI)  ;  G3 ~ GAMMA(CI)  ;   PROC1810
              G6 ~ GAMMA(CI-AI)  ;  G7 ~ GAMMA(CI-BI)  ;                PROC1820
              IF TIN = 1 THEN GO TO F1  ;                               PROC1830
              IF TIN = 2 THEN GO TO F2  ;                               PROC1840
              IF TIN = 3 THEN GO TO F3  ;                               PROC1850
              IF TIN = 4 THEN GO TO F4  ;                               PROC1860
              IF MIN = C1 THEN GO TO F1  ;  IF MIN = C2 THEN GO TO F2  ;PROC1870
              IF MIN = C3 THEN GO TO F3  ;  IF MIN = C4 THEN GO TO F4  ;PROC1880
                                                                        PROC1890
F1:           IF C1 } 1. THEN GO TO IERR  ;                             PROC1900
                                                                        PROC1910
              POWC (1-ZR,-ZI,CI-AI-BI,A,B)  ;                           PROC1920
                                                                        PROC1930
              HYPER1 (AI,BI,AI+BI-CI+1.,1.-ZR,-ZI,CMAX,EPS,PR1,PU1)  ;  PROC1940
                                                                        PROC1950
              HYPER1 (CI-AI,CI-BI,CI-AI-BI+1.,1.-ZR,-ZI,CMAX,EPS,PR2,   PROC1960
              PU2)  ;                                                   PROC1970
                                                                        PROC1980
              AG1 ~ (G3|G8)/(G6|G7)  ;  AG2 ~ (G3|G9)/(G1|G2)  ;        PROC1990
              FR ~ AG2|(A|PR2-B|PU2)+AG1|PR1  ;                         PROC2000
              FI ~ AG2|(A|PU2+B|PR2)+AG1|PU1  ;  GO TO RETURN  ;        PROC2010
                                                                        PROC2020
F2:           IF C6 { 1. THEN GO TO IERR  ;  A ~ ZR/C6*2 ; B ~ -ZI/C6*2;PROC2030
                                                                        PROC2040
              POWC (-ZR,-ZI,-AI,A1,B1)  ;  POWC (-ZR,-ZI,-BI,A2,B2)  ;  PROC2050
                                                                        PROC2060
              HYPER1 (AI,1.-CI+AI,1.-BI+AI,A,B,CMAX,EPS,PR1,PU1)  ;     PROC2070
                                                                        PROC2080
              HYPER1 (BI,1.-CI+BI,1.-AI+BI,A,B,CMAX,EPS,PR2,PU2)  ;     PROC2090
                                                                        PROC2100
              GO TO F3A  ;                                              PROC2110
                                                                        PROC2120
F3:           IF C1 { 1. THEN GO TO IERR  ;                             PROC2130
                                                                        PROC2140
              POWC (1.-ZR,-ZI,-AI,A1,B1)  ;  POWC (1.-ZR,-ZI,-BI,A2,B2);PROC2150
              AG1 ~ C1*2  ;  A ~ (1.-ZR)/AG1  ;  B ~ ZI/AG1  ;          PROC2160
                                                                        PROC2170
              HYPER1 (AI,CI-BI,AI-BI+1.,A,B,CMAX,EPS,PR1,PU1)  ;        PROC2180
                                                                        PROC2190
              HYPER1 (BI,CI-AI,BI-AI+1.,A,B,CMAX,EPS,PR2,PU2)  ;        PROC2200
                                                                        PROC2210
F3A:          AG1 ~ (G3|G5)/(G2|G6)  ;  AG2 ~ (G3|G4)/(G1|G7)  ;        PROC2220
              ALR ~ AG1|A1  ;  ALI ~ AG1|B1  ;                          PROC2230
              ANYR ~ AG2|A2  ;  ANYI ~ AG2|B2  ;                        PROC2240
              FR ~ ANYR|PR2-ANYI|PU2 + ALR|PR1-ALI|PU1  ;               PROC2250
              FI ~ ANYR|PU2+ANYI|PR2 + ALR|PU1+ALI|PR1  ;  GO TO RETURN;PROC2260
                                                                        PROC2270
F4:           IF ZR { .5 THEN GO TO IERR  ;                             PROC2280
                                                                        PROC2290
              POWC (ZR,ZI,-AI,A1,B1)  ;  POWC (ZR,ZI,AI-CI,A2,B2)  ;    PROC2300
                                                                        PROC2310
              POWC (1.-ZR,-ZI,CI-AI-BI,A3,B3)  ;                        PROC2320
                                                                        PROC2330
              AG1 ~ C6*2  ;  A ~ 1.-ZR/AG1  ;  B ~ ZI/AG1  ;            PROC2340
                                                                        PROC2350
              HYPER1 (AI,AI+1.-CI,AI+BI+1.-CI,A,B,CMAX,EPS,PR1,PU1)  ;  PROC2360
                                                                        PROC2370
              HYPER1 (CI-AI,1.-AI,CI+1.-AI-BI,A,B,CMAX,EPS,PR2,PU2)  ;  PROC2380
                                                                        PROC2390
              AG1 ~ (G3|G8)/(G6|G7)  ;  AG2 ~ (G3|G9)/(G1|G2)  ;        PROC2400
              ALR ~ AG1|A1  ;  ALI ~ AG1|B1  ;                          PROC2410
              ANYR ~ (A2|A3-B2|B3)|AG2  ;  ANYI ~ (A2|B3+B2|A3)|AG2  ;  PROC2420
              FR ~  ANYR|PR2-ANYI|PU2 + ALR|PR1-ALI|PU1  ;              PROC2430
              FI ~  ANYR|PU2+ANYI|PR2 + ALR|PU1+ALI|PR1  ; GO TO RETURN;PROC2440
                                                                        PROC2450
F5:           IF ZR } .5 THEN GO TO IERR  ;                             PROC2460
                                                                        PROC2470
              POWC (1.-ZR,-ZI,-AI,A,B)  ;                               PROC2480
                                                                        PROC2490
              HYPER1 (AI,CI-BI,CI,(ZR|(ZR-1)+ZI*2)/C1*2,-ZI/C1*2,       PROC2500
                      CMAX,EPS,PR1,PU1)  ;                              PROC2510
                                                                        PROC2520
              FR ~ PR1|A - PU1|B  ;                                     PROC2530
              FI ~ PR1|B + PU1|A  ;  GO TO RETURN  ;                    PROC2540
                                                                        PROC2550
F6:           IF C6 } 1. THEN GO TO IERR  ;                             PROC2560
                                                                        PROC2570
              HYPER1 (AI,BI,CI,ZR,ZI,CMAX,EPS,FR,FI)  ;                 PROC2580
RETURN:  END  ;                                                         PROC2590
                                                                        MAIN0420
START:        READ (FIN,FREAD,LIST1) [FINISH]  ;                        MAIN0430
                                                                        MAIN0440
              WRITE (FLO,OPENFILE,LIST1)  ;                             MAIN0450
                                                                        MAIN0460
              ANALYTIC  (AI,BI,CI,ZR,ZI,EPS,CMAX,TIN,FR,FI)  ;          MAIN0470
                                                                        MAIN0480
              WRITE (FLO,FANS,FR,FI)  ;  GO TO START  ;                 MAIN0490
                                                                        MAIN0500
EXCMAX:       WRITE (FLO,FCMAX)  ;  GO TO START  ;                      MAIN0510
                                                                        MAIN0520
ZERODEN:      WRITE (FLO,FBOG)  ;  GO TO START  ;                       MAIN0530
                                                                        MAIN0540
ERROR:        WRITE (FLO,FERR)  ;  GO TO START  ;                       MAIN0550
                                                                        MAIN0560
IERR:         WRITE (FLO,FARG)  ;  GO TO START  ;                       MAIN0570
                                                                        MAIN0580
FINISH:  END  .                                                         MAIN0590
