         BEGIN                                                          MAIN0010
                                                                        MAIN0020
COMMENT       THIS PROGRAM CONTAINS TWO PROCEDURES FOR EVALUATING       MAIN0030
              GENERALIZED HYPERGEOMETRIC FUNCTIONS . HYPER1 IS USED WHENMAIN0040
              ALL THE PARAMETERS ARE REAL BUT THE ARGUEMENT MAY BE      MAIN0050
              COMPLEX . HYPER2 IS USED WHEN ANY OF THE PARAMETERS ARE   MAIN0060
              COMPLEX REGARDLESS OF THE ARGUEMENT BEING REAL OR NOT .   MAIN0070
                                                                        MAIN0080
              ROBERT R. HAMMERS ,                                       MAIN0090
              BURROUGHS CORPORATION ,                                   MAIN0100
              (PROFESSIONAL SERVICES) ,                                 MAIN0110
              PASADENA , CALIFORNIA .                                   MAIN0120
                                                                        MAIN0130
                                                                        MAIN0140
              CARD SEQUENCE FOR MAIN PROGRAM BEGINS WITH MAIN0010 .     MAIN0150
              CARD SEQUENCE FOR HYPER1 BEGINS WITH HYP10010 .           MAIN0160
              CARD SEQUENCE FOR HYPER2 BEGINS WITH HYP20010 .           MAIN0170
                                                                        MAIN0180
              FIRST RELEASE 10 - 31 - 63 .  ;                           MAIN0190
                                                                        MAIN0200
                                                                        MAIN0210
FILE IN       F (1,10)       ;                                          MAIN0220
                                                                        MAIN0230
FILE OUT      O (1,15)       ;                                          MAIN0240
                                                                        MAIN0250
REAL          SUM,ISUM,X,Y,EPS,TR,TF,T1,T2  ;                           MAIN0260
INTEGER       CMAX,I,P,Q,NUM  ;                                         MAIN0270
BOOLEAN       B1  ;                                                     MAIN0280
LABEL         START,FINISH,L ;                                          MAIN0290
MONITOR       O (TR,TF,T1,T2)  ;                                        MAIN0300
                                                                        MAIN0310
FORMAT IN     F1 (2I3,I5,L5,3F20.10)  ,  F2 (4E20.12)  ;                MAIN0320
                                                                        MAIN0330
FORMAT OUT    FORM1 ("AN ALPHA TERM GOES TO 0 , THE ANSWER IS THEREFORE"MAIN0340
                     ," EXACT"//"REAL SUM =",E25.12/"IMAG. SUM =",      MAIN0350
                     E24.12//)  ,                                       MAIN0360
              FORM2 ("A GAMMA TERM GOES TO 0 , THE SERIES DIVERGES TO ",MAIN0370
                     "INFINITY , VALUE OF SERIES THUS FAR"/"REAL SUM =",MAIN0380
                     E25.12/"IMAG. SUM =",E24.12//)  ,                  MAIN0390
              FORM3 ("CMAX =",I4," IS EXCEEDED , VALUE OF SERIES THUS ",MAIN0400
                     "FAR"/"REAL SUM =",E25.12/"IMAG.SUM =",E24.12//)  ,MAIN0410
              FORM4 (X38,"TEST PROBLEM",I3)  ,                          MAIN0415
              O4 ("P GREATER THAN Q+1"//)  ,                            MAIN0420
              O5 ("ARGUEMENT OUT OF RANGE"//)  ,                        MAIN0430
              O10("SERIES CONVERGES TO WITHIN EPSILON"/                 MAIN0440
              "REAL SUM =",E18.10,X10,"IMAGINARY SUM =",E18.10///)     ,MAIN0450
              O11("  R U N   C O M P L E T E")                         ;MAIN0460
                                                                        MAIN0470
COMMENT       P    = NUMBER OF REAL OR IMAGINARY NUMERATOR PARAMETERS . MAIN0480
              Q    = NUMBER OF REAL OR IMAGINARY DENOMINATOR PARAMETERS.MAIN0490
              CMAX = MAXIMUM NUMBER OF TERMS OF SERIES USED .           MAIN0500
              B1   = BOOLEAN VARIABLE . IF FALSE THEN HYPER1 USED .     MAIN0510
                                        IF TRUE THEN HYPER2 USED .      MAIN0520
              X    = REAL PART OF ARGUEMENT .                           MAIN0530
              Y    = IMAGINARY PART OF ARGUEMENT .                      MAIN0540
              EPS  = RELATIVE ERROR TOLERANCE .   ;                     MAIN0550
                                                                        MAIN0560
              TR ~ TIME(1)/60  ;  NUM ~ 0  ;                            MAIN0570
                                                                        MAIN0580
START:        READ (F,F1,P,Q,CMAX,B1,X,Y,EPS) [FINISH]  ;               MAIN0590
              IF (NUM MOD 2 = 0) AND (NUM ! 0) THEN WRITE (O[PAGE])  ;  MAIN0593
              NUM ~ NUM + 1  ;  WRITE (O,FORM4,NUM)  ;                  MAIN0597
              IF P > Q+1 THEN WRITE (O,O4) ELSE                         MAIN0600
              IF P = Q+1 AND SQRT(X*2 + Y*2) } 1 THEN WRITE (O,O5) ELSE MAIN0610
              GO TO L  ;  GO TO START   ;                               MAIN0620
                                                                        MAIN0630
L:       BEGIN                                                          MAIN0640
                                                                        MAIN0650
REAL ARRAY    ALPHR,ALPHI[0:P],GAR,GAU[0:Q]    ;                        MAIN0660
LABEL         ZERODEN,ZERONUM,EXCEEDCMAX  ;                             MAIN0670
                                                                        MAIN0680
COMMENT       ALPHR[1:P] CONTAINS REAL PART OF NUMERATOR PARAMETERS .   MAIN0690
              ALPHI[1:P] CONTAINS CORRESPONDING IMAGINARY COMPONENTS    MAIN0700
                          OF NUMERATOR PARAMETERS .                     MAIN0710
              GAR[1:Q] CONTAINS REAL PART OF DENOMINATOR PARAMETERS .   MAIN0720
              GAU[1:Q] CONTAINS CORRESPONDING IMAGINARY COMPONENTS OF   MAIN0730
                          DENOMINATOR PARNMETERS .                      MAIN0740
              USER MUST PROVIDE FOLLOWING THREE LABELS IN MAIN PROGRAM  MAIN0750
              WHEN USING HYPER1 OR HYPER2:                              MAIN0760
              EXCEEDCMAX - CONTROL TRANSFERRED TO HERE IF NUMBER OF     MAIN0770
                           TERMS CALCULATED EXCEEDS CMAX .              MAIN0780
              ZERONUM    - CONTROL TRANSFERRED TO HERE IF SERIES        MAIN0790
                           TERMINATES , ALL REMAINING TERMS ARE ZERO .  MAIN0800
              ZERODEN    - CONTROL TRANSFERRED TO HERE IF SERIES        MAIN0810
                           DIVERGES BECAUSE OF ZERO DENOMINATOR PARAM. ;MAIN0820
                                                                        MAIN0830
PROCEDURE     DUMPIN  ;                                                 MAIN0840
                                                                        MAIN0850
         BEGIN                                                          MAIN0860
LABEL         XX  ;                                                     MAIN0870
DUMP          O (P,Q,CMAX,B1,X,Y,EPS,ALPHR,ALPHI,GAR,GAU) XX:1  ;       MAIN0880
              P ~ P  ;                                                  MAIN0890
                                                                        MAIN0900
XX:      END  ;                                                         MAIN0910
                                                                        MAIN0920
PROCEDURE     HYPER1 (P,Q,ALPHR,GAR,X,Y,EPS,CMAX,SUM,ISUM,              HYP10010
              EXCEEDCMAX,ZERODEN,ZERONUM)  ;                            HYP10020
                                                                        HYP10030
VALUE         P,Q,X,Y,EPS,CMAX  ;                                       HYP10040
REAL          X,Y,EPS,SUM,ISUM  ;                                       HYP10050
INTEGER       P,Q,CMAX          ;                                       HYP10060
ARRAY         ALPHR,GAR[0]      ;                                       HYP10070
LABEL         EXCEEDCMAX,ZERODEN,ZERONUM  ;                             HYP10080
                                                                        HYP10090
         BEGIN                                                          HYP10100
                                                                        HYP10110
REAL          HYP,HYI,ATERM,GTERM,TEM1,TEM2,LEASTMIN  ;                 HYP10120
INTEGER       I,COUNT  ;                                                HYP10130
LABEL         RECURSE,EXIT,TESTI  ;                                     HYP10140
                                                                        HYP10150
              LEASTMIN ~ 0  ;                                           HYP10160
              HYP ~ SUM ~ 1  ;  HYI ~ ISUM ~ COUNT ~ 0  ;               HYP10170
                                                                        HYP10180
              FOR I ~ 1 STEP 1 UNTIL P DO IF (ATERM ~ ALPHR[I]) <       HYP10190
                      LEASTMIN THEN LEASTMIN ~ ATERM  ;                 HYP10200
                                                                        HYP10210
              FOR I ~ 1 STEP 1 UNTIL Q DO IF (GTERM ~ GAR[I]) <         HYP10220
                      LEASTMIN THEN LEASTMIN ~ GTERM  ;                 HYP10230
              LEASTMIN ~ -LEASTMIN + 2  ;                               HYP10240
                                                                        HYP10250
RECURSE:      ATERM ~ GTERM ~ 1  ;  FOR I ~ 1 STEP 1 UNTIL P DO         HYP10260
              ATERM ~ ATERM|(ALPHR[I]+COUNT)  ;                         HYP10270
              IF ATERM = 0 THEN                                         HYP10280
         BEGIN                                                          HYP10290
              FOR I ~ COUNT+1 STEP 1 UNTIL Q DO IF (GTERM ~ GAR[I]) =   HYP10300
              ENTIER(GTERM) AND GTERM { 0 THEN                          HYP10310
              GO TO ZERODEN  ;  GO TO ZERONUM                           HYP10320
         END  ;                                                         HYP10330
                                                                        HYP10340
              FOR I ~ 1 STEP 1 UNTIL Q DO                               HYP10350
              GTERM ~ GTERM|(GAR[I]+COUNT)    ;                         HYP10360
              IF GTERM = 0 THEN                                         HYP10370
              GO TO ZERODEN  ;                                          HYP10380
              COUNT ~ COUNT+1  ;                                        HYP10390
              IF Y ! 0 THEN                                             HYP10400
         BEGIN                                                          HYP10410
              TEM1 ~ ATERM/(GTERM|COUNT)   ;                            HYP10420
              TEM2 ~ (HYP|X-Y|HYI)|TEM1   ;                             HYP10430
              HYI  ~ (HYP|Y+X|HYI)|TEM1   ;   HYP ~ TEM2        ;       HYP10440
              SUM  ~  HYP  +  SUM         ;  ISUM ~ HYI + ISUM  ;       HYP10450
              IF COUNT < LEASTMIN THEN GO TO RECURSE  ;                 HYP10460
              IF SUM = 0 THEN                                           HYP10470
         BEGIN                                                          HYP10480
              IF ISUM ! 0 THEN                                          HYP10490
         BEGIN                                                          HYP10500
TESTI:        IF ABS( HYI /ISUM) < EPS THEN GO TO EXIT                  HYP10510
         END  ;                                                         HYP10520
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCEEDCMAX   HYP10530
         END  ;                                                         HYP10540
              IF ABS( HYP /SUM) < EPS THEN                              HYP10550
         BEGIN                                                          HYP10560
              IF ISUM = 0 THEN GO TO EXIT  ;  GO TO TESTI               HYP10570
         END  ;                                                         HYP10580
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCEEDCMAX  ;HYP10590
         END  ;                                                         HYP10600
              HYP ~ ATERM/(GTERM|COUNT)|X|HYP  ;                        HYP10610
              SUM ~ SUM + HYP  ;                                        HYP10620
              IF SUM = 0 THEN                                           HYP10630
         BEGIN                                                          HYP10640
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCEEDCMAX   HYP10650
         END  ;                                                         HYP10660
              IF ABS(HYP/SUM) < EPS THEN GO TO EXIT  ;                  HYP10670
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCEEDCMAX  ;HYP10680
                                                                        HYP10690
EXIT:    END  ;                                                         HYP10700
                                                                        MAIN0930
PROCEDURE     HYPER2 (P,Q,ALPHR,ALPHI,GAR,GAU,X,Y,EPS,CMAX,SUM,ISUM,    HYP20010
              EXCEEDCMAX,ZERODEN,ZERONUM)  ;                            HYP20020
                                                                        HYP20030
VALUE         P,Q,X,Y,EPS,CMAX  ;                                       HYP20040
REAL          X,Y,EPS,SUM,ISUM  ;                                       HYP20050
INTEGER       P,Q,CMAX          ;                                       HYP20060
ARRAY         ALPHR,ALPHI,GAR,GAU[0]  ;                                 HYP20070
LABEL         EXCEEDCMAX,ZERODEN,ZERONUM  ;                             HYP20080
                                                                        HYP20090
         BEGIN                                                          HYP20100
                                                                        HYP20110
REAL          ATERMR,ATERMI,GTERMR,GTERMI,TEMP,TEM1,TEM2,PARTR,PARTI,   HYP20120
              LEASTMIN  ;                                               HYP20130
INTEGER       I,COUNT  ;                                                HYP20140
LABEL         RECURSE,EXIT,TESTI  ;                                     HYP20150
                                                                        HYP20160
              LEASTMIN ~ 0  ;                                           HYP20170
                                                                        HYP20180
              FOR I ~ 1 STEP 1 UNTIL P DO IF (ATERMR~ ALPHR[I]) <       HYP20190
                      LEASTMIN THEN LEASTMIN ~ ATERMR ;                 HYP20200
                                                                        HYP20210
              FOR I ~ 1 STEP 1 UNTIL Q DO IF (GTERMR~ GAR[I]) <         HYP20220
                      LEASTMIN THEN LEASTMIN ~ GTERMR ;                 HYP20230
              LEASTMIN ~ -LEASTMIN + 2  ;                               HYP20240
              ISUM ~ PARTI ~ COUNT ~ 0  ;  SUM ~ PARTR ~ 1  ;           HYP20250
RECURSE:      IF P ! 0 THEN                                             HYP20260
         BEGIN                                                          HYP20270
              ATERMR ~ ALPHR[1]+COUNT  ;  ATERMI ~ ALPHI[1]  ;          HYP20280
                                                                        HYP20290
              FOR I ~ 2 STEP 1 UNTIL P DO                               HYP20300
         BEGIN                                                          HYP20310
              TEM1 ~ ALPHR[I]+COUNT  ;  TEM2 ~ ALPHI[I]  ;              HYP20320
              TEMP ~ ATERMR|TEM1-ATERMI|TEM2  ;                         HYP20330
              ATERMI ~ ATERMR|TEM2+ATERMI|TEM1  ;                       HYP20340
              ATERMR ~ TEMP                                             HYP20350
         END  ;                                                         HYP20360
              IF ATERMR = 0 AND ATERMI = 0 THEN                         HYP20370
         BEGIN                                                          HYP20380
                                                                        HYP20390
              FOR I ~ COUNT+1 STEP 1 UNTIL Q DO IF GAU[I] = 0 THEN      HYP20400
              IF (GTERMR ~ GAR[I]) = ENTIER(GTERMR) AND GTERMR { 0 THEN HYP20410
              GO TO ZERODEN  ;  GO TO ZERONUM                           HYP20420
         END                                                            HYP20430
         END  ELSE                                                      HYP20440
         BEGIN                                                          HYP20450
              ATERMR ~ 1  ;  ATERMI ~ 0                                 HYP20460
         END  ;                                                         HYP20470
              IF Q ! 0 THEN                                             HYP20480
         BEGIN                                                          HYP20490
              GTERMR ~ GAR[1] + COUNT  ;  GTERMI ~ GAU[1]  ;            HYP20500
                                                                        HYP20510
              FOR I ~ 2 STEP 1 UNTIL Q DO                               HYP20520
         BEGIN                                                          HYP20530
              TEM1 ~ GAR[I]+COUNT  ;  TEM2 ~ GAU[I]  ;                  HYP20540
              TEMP ~ GTERMR|TEM1-GTERMI|TEM2  ;                         HYP20550
              GTERMI ~ GTERMR|TEM2+GTERMI|TEM1  ;                       HYP20560
              GTERMR ~ TEMP                                             HYP20570
         END  ;                                                         HYP20580
              IF GTERMR = 0 AND GTERMI = 0 THEN GO TO ZERODEN           HYP20590
         END  ELSE                                                      HYP20600
         BEGIN                                                          HYP20610
              GTERMR ~ 1  ;  GTERMI ~ 0                                 HYP20620
         END  ;                                                         HYP20630
              COUNT  ~ COUNT+1 ; TEM1 ~ 1/((GTERMR*2+GTERMI*2)|COUNT)  ;HYP20640
              TEMP   ~ ATERMR|GTERMR + ATERMI|GTERMI   ;                HYP20650
              ATERMI ~ ATERMI|GTERMR - ATERMR|GTERMI   ;                HYP20660
              ATERMR ~ TEMP  |PARTR  - ATERMI|PARTI    ;                HYP20670
              ATERMI ~ TEMP  |PARTI  + ATERMI|PARTR    ;                HYP20680
              PARTR  ~ (X    |ATERMR - ATERMI|Y)|TEM1  ;                HYP20690
              PARTI  ~ (Y    |ATERMR + ATERMI|X)|TEM1  ;                HYP20700
               SUM   ~ PARTR +  SUM  ;  ISUM ~ PARTI + ISUM  ;          HYP20710
              IF COUNT < LEASTMIN THEN GO TO RECURSE  ;                 HYP20720
              IF SUM = 0 THEN                                           HYP20730
         BEGIN                                                          HYP20740
              IF ISUM ! 0 THEN                                          HYP20750
         BEGIN                                                          HYP20760
TESTI:        IF ABS(PARTI/ISUM) < EPS THEN GO TO EXIT                  HYP20770
         END  ;                                                         HYP20780
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCEEDCMAX   HYP20790
         END  ;                                                         HYP20800
              IF ABS(PARTR/SUM) < EPS THEN                              HYP20810
         BEGIN                                                          HYP20820
              IF ISUM = 0 THEN GO TO EXIT  ;  GO TO TESTI               HYP20830
         END  ;                                                         HYP20840
              IF COUNT < CMAX THEN GO TO RECURSE  ;  GO TO EXCEEDCMAX  ;HYP20850
                                                                        HYP20860
EXIT:    END  ;                                                         HYP20870
                                                                        MAIN0940
              IF B1 THEN                                                MAIN0950
         BEGIN                                                          MAIN0960
                                                                        MAIN0970
              READ (F,F2,FOR I ~ 1 STEP 1 UNTIL P DO [ALPHR[I],         MAIN0980
              ALPHI [I]],FOR I ~ 1 STEP 1 UNTIL Q DO [GAR  [I],         MAIN0990
              GAU   [I]])  ;                                            MAIN1000
              DUMPIN  ;                                                 MAIN1010
                                                                        MAIN1020
              HYPER2 (P,Q,ALPHR,ALPHI,GAR,GAU,X,Y,EPS,CMAX,SUM,ISUM,    MAIN1030
              EXCEEDCMAX,ZERODEN,ZERONUM)  ;                            MAIN1040
                                                                        MAIN1060
              WRITE (O,O10,SUM,ISUM)  ;  GO TO START                    MAIN1070
         END  ;                                                         MAIN1080
                                                                        MAIN1090
              READ (F,F2,FOR I ~ 1 STEP 1 UNTIL P DO ALPHR[I],          MAIN1100
                         FOR I ~ 1 STEP 1 UNTIL Q DO GAR[I])           ;MAIN1110
              DUMPIN  ;                                                 MAIN1120
                                                                        MAIN1130
              HYPER1 (P,Q,ALPHR,GAR,X,Y,EPS,CMAX,SUM,ISUM,              MAIN1140
              EXCEEDCMAX,ZERODEN,ZERONUM)  ;                            MAIN1150
                                                                        MAIN1170
              WRITE (O,O10,SUM,ISUM)  ;  GO TO START  ;                 MAIN1180
ZERONUM:      WRITE (O,FORM1,SUM,ISUM)  ;  GO TO START  ;               MAIN1190
ZERODEN:      WRITE (O,FORM2,SUM,ISUM)  ;  GO TO START  ;               MAIN1200
EXCEEDCMAX:   WRITE (O,FORM3,CMAX,SUM,ISUM)  ;  GO TO START             MAIN1210
         END  ;                                                         MAIN1220
                                                                        MAIN1230
FINISH:       WRITE (O,O11)  ;  TF ~ TIME(1)/60                         MAIN1240
                                                                        MAIN1250
         END  .                                                         MAIN1260
