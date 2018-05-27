 LABEL  0000000000XXXXXX0010000001
$ CARD
         BEGIN
FILE OUT      HAMMERS 1(2,15)  ;
REAL TTT  ;  FORMAT TTTT (/"TIME = ",F14.3," MILLI-SECONDS"/)  ;
ALPHA FILE OUT PLOTTER (1,3061)  ;                                      MAIN0310
INTEGER ARRAY  SYMBOLA[0:112],SYMBOLB[-15:63]  ;                        MAIN0320
INTEGER ARRAY A[0:1022],A1,A2[0:8]  ;                                   MAIN0330
                                                                        MAIN0340
PROCEDURE     PLOT(X,Y,IC)  ;                                           PLOT0010
                                                                        PLOT0020
REAL          X,Y  ;                                                    PLOT0030
INTEGER       IC  ;                                                     PLOT0040
                                                                        PLOT0050
         BEGIN                                                          PLOT0060
                                                                        PLOT0070
OWN INTEGER   M,MM  ;                                                   PLOT0080
              IF IC > 5 THEN                                            PLOT0090
         BEGIN                                                          PLOT0100
              IF IC > 1020 THEN M ~ 1019 ELSE M ~ IC - 1  ;             PLOT0110
              MM ~ X  ;                                                 PLOT0120
         END ;                                                          PLOT0130
                                                                        PLOT0140
         BEGIN                                                          PLOT0150
                                                                        PLOT0160
STREAM PROCEDURE   STRT(FNT,FNTS)  ;                                    PLOT0170
         BEGIN                                                          PLOT0180
                                                                        PLOT0190
LOCAL         ST  ;                                                     PLOT0200
              DI ~ LOC ST  ;  SI ~ FNT  ;  DS ~ WDS  ;                  PLOT0210
              SI ~ ST  ;  ST ~ SI  ;                                    PLOT0220
              SI ~ LOC ST  ;  DI ~ FNTS  ;  DS ~ WDS  ;                 PLOT0230
         END ;                                                          PLOT0240
                                                                        PLOT0250
STREAM PROCEDURE   TRANS(FNT,AN,MT,MT1)  ;                              PLOT0260
                                                                        PLOT0270
VALUE  MT,MT1  ;                                                        PLOT0280
         BEGIN                                                          PLOT0290
              SI ~ AN  ;  DI ~ FNT  ;                                   PLOT0300
              DI ~ DI + 5  ;  DI ~ DC  ;                                PLOT0310
              60(MT(SI ~ SI + 2  ;  DS ~ 6 CHR))  ;                     PLOT0320
              MT1(SI ~ SI + 2  ;  DS ~ 6 CHR  ;  )  ;                   PLOT0330
              MT ~ DI ;   DI ~ FNT  ;                                   PLOT0340
              SI ~ LOC MT  ;  DS ~ WDS  ;                               PLOT0350
         END ;                                                          PLOT0360
                                                                        PLOT0370
STREAM PROCEDURE   SYNC(FNT)  ;                                         PLOT0380
                                                                        PLOT0390
         BEGIN                                                          PLOT0400
              DI ~ FNT  ;                                               PLOT0410
              DI ~ DI + 5  ;  DI ~ DC  ;                                PLOT0420
              DS ~ 5 LIT "4634~"  ;                                     PLOT0430
         END ;                                                          PLOT0440
                                                                        PLOT0450
STREAM PROCEDURE   INITIALIZE(FNT,BL)  ;                                PLOT0460
                                                                        PLOT0470
         BEGIN                                                          PLOT0480
LOCAL         ST  ;                                                     PLOT0490
              DI ~ LOC ST  ;  SI ~ FNT  ;  DS ~ WDS  ;                  PLOT0500
              SI ~ BL  ;  DI ~ ST  ;  SI ~ SI + 2  ;                    PLOT0510
              DS ~ 8 LIT "44444444"  ;                                  PLOT0520
              DS ~ 8 LIT "44333333"  ;                                  PLOT0530
              DS ~ 2 LIT "31"  ;                                        PLOT0540
              DS ~ 6 CHR  ;                                             PLOT0550
              DS ~ 8 LIT "13333333"  ;                                  PLOT0560
              40(DS ~ 8 LIT "44444444")  ;                              PLOT0570
              DS ~ 5 LIT "4634~"  ;                                     PLOT0580
         END ;                                                          PLOT0590
                                                                        PLOT0600
OWN INTEGER   I,NPX,NPY,BA,T,BC,BUFS  ;                                 PLOT0610
OWN BOOLEAN   BOOL,PEN,FIXED,NEWF  ;                                    PLOT0620
OWN REAL      LNGTH,LENGTH,SF,RECORD  ;                                 PLOT0640
INTEGER         J,K,JJ,NX,NY,DX,DY,IX,IY,NR,NT,NC,II1,II2,NA,JJ1  ;     PLOT0650
INTEGER       NODEL  ;                                                  PLOT0660
LABEL         FINSH,ON,L1,L2,FIN  ;                                     PLOT0670
                                                                        PLOT0680
DEFINE        STARTPLOT =                                               PLOT0690
              A[0] ~ A[1] ~ 4363141380  ;                               PLOT0700
              A[2] ~ 4363141315  ;  A[3] ~ 3272356034  ;                PLOT0710
              STRT(PLOTTER,BUFS)  ;                                     PLOT0720
              BC ~ 1  ;  I ~ 4 #  ;                                     PLOT0730
DEFINE        TESTAPE =                                                 PLOT0740
              LENGTH ~ LENGTH + RECORD  ;                               PLOT0750
              IF LENGTH } LNGTH THEN                                    PLOT0760
         BEGIN                                                          PLOT0770
              CLOSE(PLOTTER,SAVE) ;                                     PLOT0780
              BA ~ 0  ;  LENGTH ~ 0  ;                                  PLOT0790
              GO TO FINSH  ;                                            PLOT0800
         END ; # ;                                                      PLOT0810
                                                                        PLOT0820
              IF ABS(IC) > 3 THEN                                       PLOT0830
         BEGIN                                                          PLOT0840
              IF ABS(IC) = 5 THEN                                       PLOT0850
         BEGIN                                                          PLOT0860
              NEWF ~ NOT NEWF  ;  GO TO FIN                             PLOT0870
         END ;                                                          PLOT0890
              IF ABS(IC) = 4 THEN                                       PLOT0900
         BEGIN                                                          PLOT0910
              IF IC > 0 THEN                                            PLOT0920
         BEGIN                                                          PLOT0930
              X ~ NPX/(100.0 | SF)  ;  Y ~ NPY/(100.0 | SF)  ;          PLOT0940
         END                                                            PLOT0950
              ELSE                                                      PLOT0960
              SF ~ X  ;                                                 PLOT0970
              GO TO FIN  ;                                              PLOT0980
         END ;                                                          PLOT0990
              IF NOT FIXED THEN                                         PLOT1000
         BEGIN                                                          PLOT1010
FILL A1[*] WITH    OCT50500000000,OCT50600000000,OCT50700000000  ,      PLOT1020
                   OCT60500000000,OCT60600000000,OCT60700000000  ,      PLOT1030
                   OCT70500000000,OCT70600000000,OCT70700000000  ;      PLOT1040
FILL A2[*] WITH    OCT50500,OCT50600,OCT50700,OCT60500,OCT60600,        PLOT1050
                   OCT60700,OCT70500,OCT70600,OCT70700  ;               PLOT1060
              FIXED ~ TRUE  ;                                           PLOT1070
         END ;                                                          PLOT1080
              IF Y ! 0 THEN LNGTH ~ Y ELSE LNGTH ~ 2000.0  ;            PLOT1090
              SF ~ 1.0  ;                                               PLOT1100
              RECORD ~ (6 | MM | (M + 1)/200+0.75)/12  ;  LENGTH~0  ;   PLOT1110
              NPX ~ NPY ~ 0  ;  BA ~ 1  ;                               PLOT1120
              BOOL ~ PEN ~ TRUE  ;                                      PLOT1130
              GO TO FINSH  ;                                            PLOT1140
         END ;                                                          PLOT1150
              IF BOOL THEN T ~ 1572870  ;                               PLOT1160
              IF ABS(IC) = 2 THEN                                       PLOT1170
         BEGIN                                                          PLOT1180
              IF NOT PEN THEN GO TO ON  ;                               PLOT1190
              IF BOOL THEN T ~ 1835014 ELSE T ~ T+1  ;                  PLOT1200
         END                                                            PLOT1210
              ELSE IF ABS(IC) = 3 THEN                                  PLOT1220
         BEGIN                                                          PLOT1230
              IF PEN THEN GO TO ON  ;                                   PLOT1240
              IF BOOL THEN T ~ 1310726 ELSE T ~ T- 1  ;                 PLOT1250
         END                                                            PLOT1260
              ELSE GO TO ON  ;                                          PLOT1270
              PEN ~ NOT PEN  ;                                          PLOT1280
              A[I] ~ IF BOOL THEN T+6543139200 ELSE T + 24960  ;        PLOT1290
              BOOL ~ TRUE  ;                                            PLOT1300
              I ~ I + 1  ;                                              PLOT1310
              IF NOT NEWF THEN                                          PLOT1320
              NODEL ~ IF PEN THEN 1 ELSE 7  ;                           PLOT1330
              FOR K ~ 1 STEP 1 UNTIL NODEL DO                           PLOT1340
         BEGIN                                                          PLOT1350
              IF I > M THEN                                             PLOT1360
         BEGIN                                                          PLOT1370
              JJ ~ (M + 1) MOD 60  ;                                    PLOT1380
              TRANS(BUFS,A,ENTIER((M + 1)/60),JJ)  ;                    PLOT1390
              IF BC = MM THEN                                           PLOT1400
         BEGIN                                                          PLOT1410
              SYNC(BUFS)  ;                                             PLOT1420
              RELEASE(PLOTTER)  ;  TESTAPE  ;  STARTPLOT  ;             PLOT1430
         END                                                            PLOT1440
              ELSE                                                      PLOT1450
         BEGIN                                                          PLOT1460
              I ~ 0  ;  BC ~ BC + 1  ;                                  PLOT1470
         END ;                                                          PLOT1480
         END ;                                                          PLOT1490
              A[I] ~ 6544712070  ;  I ~ I + 1  ;                        PLOT1500
         END ;                                                          PLOT1510
              T ~ 1572870  ;                                            PLOT1520
              I ~ I - 1  ;                                              PLOT1530
ON:           NX ~ 100.0 | X | SF  ;  NY ~ 100.0 | Y | SF  ;            PLOT1540
              DX ~ NX - NPX  ;  DY ~ NY - NPY  ;                        PLOT1550
              NPX ~ NX  ;  NPY ~ NY  ;                                  PLOT1560
              IF DX } 0 THEN                                            PLOT1570
              IF DX = 0 THEN IX ~ 3 ELSE IX ~ 6 ELSE IX ~ 0  ;          PLOT1580
              IF DY } 0 THEN                                            PLOT1590
              IF DY = 0 THEN IY ~ IX + 1 ELSE IY ~ IX + 2 ELSE IY ~ IX ;PLOT1600
              IF ABS(DX) } ABS(DY) THEN                                 PLOT1610
         BEGIN                                                          PLOT1620
              NR ~ ABS(DY)  ;  NC ~ NT ~ ABS(DX)  ;                     PLOT1630
              IX ~ IX + 1  ;                                            PLOT1640
         END                                                            PLOT1650
              ELSE                                                      PLOT1660
         BEGIN                                                          PLOT1670
              NR ~ ABS(DX)  ;  NC ~ NT ~ ABS(DY)  ;                     PLOT1680
              IX ~ IY - IX + 3  ;                                       PLOT1690
         END ;                                                          PLOT1700
              NA ~ NT DIV 2  ;                                          PLOT1710
L1:           IF NC ! 0 THEN                                            PLOT1720
         BEGIN                                                          PLOT1730
              NA ~ NA + NR  ;                                           PLOT1740
              IF NA } NT THEN                                           PLOT1750
         BEGIN                                                          PLOT1760
              IF BOOL THEN T ~ T + A1[IY]                               PLOT1770
              ELSE T ~ T + A2[IY]  ;                                    PLOT1780
              NA ~ NA - NT  ;                                           PLOT1790
         END                                                            PLOT1800
              ELSE                                                      PLOT1810
         BEGIN                                                          PLOT1820
              IF BOOL THEN T ~ T + A1[IX]                               PLOT1830
              ELSE T ~ T + A2[IX]  ;                                    PLOT1840
         END ;                                                          PLOT1850
              BOOL ~ NOT BOOL  ;                                        PLOT1860
              IF BOOL THEN                                              PLOT1870
         BEGIN                                                          PLOT1880
              A[I] ~ T  ;  I ~ I +1  ;                                  PLOT1890
              T ~ 1572870  ;                                            PLOT1900
              IF I > M THEN                                             PLOT1910
         BEGIN                                                          PLOT1920
              JJ ~ (M + 1) MOD 60  ;                                    PLOT1930
              TRANS(BUFS,A,ENTIER((M + 1)/60),JJ)  ;                    PLOT1940
              IF BC = MM THEN                                           PLOT1950
         BEGIN                                                          PLOT1960
              SYNC(BUFS)  ;                                             PLOT1970
              RELEASE(PLOTTER)  ;  TESTAPE  ;  STARTPLOT  ;             PLOT1980
         END                                                            PLOT1990
              ELSE                                                      PLOT2000
         BEGIN                                                          PLOT2010
              I ~ 0  ;  BC ~ BC + 1  ;                                  PLOT2020
         END ;                                                          PLOT2030
         END ;                                                          PLOT2040
         END ;                                                          PLOT2050
              NC ~ NC - 1  ;  GO TO L1  ;                               PLOT2060
         END ;                                                          PLOT2070
              IF IC < 0 THEN                                            PLOT2080
         BEGIN                                                          PLOT2090
              IF NOT BOOL THEN A[I] ~ T + 24960 ELSE I ~ I - 1  ;       PLOT2100
L2:           IF I MOD 4 ! 3 THEN                                       PLOT2110
         BEGIN                                                          PLOT2120
              A[I ~ I + 1] ~ 6544712070   ;  GO TO L2  ;                PLOT2130
         END ;                                                          PLOT2140
              I ~ I + 1  ;                                              PLOT2150
              JJ ~ I MOD 60  ;                                          PLOT2160
              TRANS(BUFS,A,ENTIER(I/60),JJ)  ;                          PLOT2170
              SYNC(BUFS)  ;                                             PLOT2180
              RELEASE(PLOTTER)  ;                                       PLOT2190
              BOOL ~ TRUE  ;                                            PLOT2200
              NPX ~ NPY ~ 0  ;                                          PLOT2210
FINSH:        JJ ~ BA  ;  NA ~ 0  ;                                     PLOT2220
              FOR K ~ 0,2,4 DO                                          PLOT2230
         BEGIN                                                          PLOT2240
              J ~ JJ MOD 10  ;  JJ ~ JJ DIV 10  ;                       PLOT2250
              NA ~ NA + ((4 + J MOD 4) + (4 + J DIV 4) | 64) | 64 * K  ;PLOT2260
         END  ;                                                         PLOT2270
              BA ~ BA + 1  ;                                            PLOT2280
              INITIALIZE (PLOTTER,NA)  ;                                PLOT2290
              RELEASE(PLOTTER)  ;                                       PLOT2300
              STARTPLOT  ;                                              PLOT2310
         END ;                                                          PLOT2320
FIN:     END ;                                                          PLOT2330
         END PLOT PROCEDURE  ;                                          PLOT2340
                                                                        MAIN0350
PROCEDURE     SYMBOL(X0, Y0, HGT, BCD, THETA, N) ;                      SYMB0010
                                                                        SYMB0020
VALUE         X0, Y0, HGT, THETA, N ;                                   SYMB0030
INTEGER       N ;                                                       SYMB0040
REAL          X0, Y0, HGT, THETA ;                                      SYMB0050
ALPHA ARRAY   BCD[0] ;                                                  SYMB0060
                                                                        SYMB0070
         BEGIN                                                          SYMB0080
                                                                        SYMB0090
INTEGER       BINX, AC, W, OSC, AINX, I, OSTS, OSTS1, MOVE,INT  ;       SYMB0100
REAL          XA,YA,X,Y,XN,YN ;                                         SYMB0110
BOOLEAN       LP ;                                                      SYMB0120
OWN BOOLEAN   FIXED  ;                                                  SYMB0130
DEFINE   A = SYMBOLA  #,  B = SYMBOLB  #  ;                             SYMB0140
LABEL         L1,L2,L3,L4,L5,LOADB,S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,  SYMB0150
              S12,S13,S14,S15,T1, M1,Y1,EL,EXIT, SKIP ;                 SYMB0160
SWITCH        SW ~ L1, L2, L3, L4, L5 ;                                 SYMB0170
SWITCH        SW1 ~ S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15 ;SYMB0180
DEFINE        D1 = GO TO LOADB#, D2 = GO TO M1# ;                       SYMB0190
                                                                        SYMB0200
              IF NOT FIXED THEN                                         SYMB0210
              FILL A[*] WITH OCT103041463717060, OCT110000000000000,    SYMB0220
              OCT103020271600000,OCT400001444637170,OCT605000000000000, SYMB0230
              OCT011030414334143,OCT445463717060000,OCT070343333730204, SYMB0240
              OCT000000000000000,OCT011030414334040,OCT747000000000000, SYMB0250
              OCT031434434130100,OCT106173746000000,OCT060747212000000, SYMB0260
              OCT344341301001031,OCT434454637170605,OCT140000000000000, SYMB0270
              OCT011030414637170,OCT604133344000000,OCT111514041412024, SYMB0280
              OCT232313534440400,OCT313313114144040,OCT040000000000000, SYMB0290
              OCT000000000000000,OCT101121201070222,OCT345463717060000, SYMB0300
              OCT111222211170141,OCT525241400000000,OCT024406000000000, SYMB0310
              OCT014170064402000,OCT212523034300000,OCT000343463717060, SYMB0320
              OCT343400000000000,OCT040737464534040,OCT030414334000000, SYMB0330
              OCT424130100106173,OCT746450000000000,OCT000737464130000, SYMB0340
              OCT470704340400400,OCT470704340400000,OCT334341301001061, SYMB0350
              OCT737464500000000,OCT000704444740000,OCT103020271737000, SYMB0360
              OCT102021111000000,OCT301017370000000,OCT362717060540314, SYMB0370
              OCT220100103140000,OCT301215370000000,OCT460442000000000, SYMB0380
              OCT440415130400000,OCT014523054100000,OCT011030414700000, SYMB0390
              OCT000703472540000,OCT070040000000000,OCT000723474000000, SYMB0400
              OCT000740470000000,OCT103041463770364,OCT770371706011000, SYMB0410
              OCT000737464534040,OCT100106173746413,OCT010702240000000, SYMB0420
              OCT000737464534043,OCT443400000000000,OCT023243341405164, SYMB0430
              OCT626272000000000,OCT014523054123034,OCT323252100000000, SYMB0440
              OCT034300000000000,OCT103235170000000,OCT102122121121702, SYMB0450
              OCT425151424000000,OCT014170420446000,OCT000000000000000, SYMB0460
              OCT004700000000000,OCT011030414334140,OCT506173746000000, SYMB0470
              OCT202707470000000,OCT070110304147000,OCT072047000000000, SYMB0480
              OCT070024404700000,OCT004770074000000,OCT072547252000000, SYMB0490
              OCT074724143424004,OCT000000000000000,OCT102122121121000, SYMB0500
              OCT004770160607171,OCT670413130404100,OCT024635450535130, SYMB0510
              OCT343000000000000,OCT024270044400000,OCT103037170000000, SYMB0520
              OCT141670363400000,OCT004044040004242,OCT200000000000000, SYMB0530
              OCT004422044022000,OCT004000440444220,OCT004004440022000, SYMB0540
              OCT202244220422000,OCT222422000000000,OCT240141242200000, SYMB0560
              OCT243443413010010,OCT314242200000000,OCT244220022422000, SYMB0550
              OCT240242242022000,OCT443313041311001,OCT131403133220000, SYMB0570
              OCT103650046410702,OCT222000000000000,OCT024222202422000, SYMB0580
              OCT004404402200000,OCT004422242022044,OCT022024222000000 ;SYMB0590
T1:           IF NOT FIXED THEN                                         SYMB0600
              FILL B[*] WITH OCT30157,OCT12156,OCT14155,OCT22153,       SYMB0610
              OCT32151,OCT14150,OCT12147,OCT06146,OCT14145,OCT14144,    SYMB0620
              OCT26142,OCT14141,OCT16140,OCT14137,OCT20135,OCT22000,    SYMB0630
              OCT12002,OCT22003,OCT32005,OCT20007,OCT22011,OCT30013,    SYMB0640
              OCT12015,OCT40016,OCT30021,OCT34023,OCT22025,OCT32030,    SYMB0650
              OCT26032,OCT06034,OCT14035,OCT12036,OCT24037,OCT30041,    SYMB0660
              OCT24043,OCT16045,OCT16046,OCT14047,OCT26050,OCT14052,    SYMB0670
              OCT14053,OCT12054,OCT10055,OCT32056,OCT10060,OCT06061,    SYMB0680
              OCT12062,OCT12063,OCT12064,OCT14065,OCT06066,OCT12067,    SYMB0690
              OCT10070,OCT34071,OCT16073,OCT30074,OCT24076,OCT26100,    SYMB0700
              OCT26102,OCT04104,OCT10105, OCT30106,OCT14110,OCT00111,   SYMB0710
              OCT04112,OCT30113,OCT10115,OCT14116,OCT06117,OCT12120,    SYMB0720
              OCT12121,OCT12122,OCT20123,OCT14125,OCT34126,OCT22130,    SYMB0730
              OCT12132,OCT10133,OCT12134 ;                              SYMB0740
              FIXED ~ TRUE  ;                                           SYMB0750
              XA ~ (HGT/7) | COS(0.01745330754 | THETA)  ;              SYMB0760
              YA ~ (HGT/7) | SIN(0.01745330754 | THETA)  ;              SYMB0770
              PLOT(X0,Y0,3) ;                                           SYMB0780
              IF N}0 THEN                                               SYMB0790
         BEGIN                                                          SYMB0800
              X ~ X0 ;  Y ~ Y0                                          SYMB0810
         END  ELSE                                                      SYMB0820
         BEGIN                                                          SYMB0830
              XA ~ .5714285714286|XA ;  YA ~ .5714285714286|YA ;        SYMB0840
              X ~ X0 - 2|XA + 2|YA ;                                    SYMB0850
              Y ~ Y0 - 2|XA - 2|YA ;                                    SYMB0860
              BINX ~ N ;  D1                                            SYMB0870
         END ;                                                          SYMB0880
              FOR AC ~ 1 STEP 1 UNTIL N DO                              SYMB0890
         BEGIN                                                          SYMB0900
              W~BCD[(AC-1) DIV 6] ;  GO TO SW[INT~AC MOD 6]  ;          SYMB0910
              BINX ~ W.[42:6] ;  D1 ;                                   SYMB0920
L1:           BINX ~ W.[12:6] ;  D1 ;                                   SYMB0930
L2:           BINX ~ W.[18:6] ;  D1 ;                                   SYMB0940
L3:           BINX ~ W.[24:6] ;  D1 ;                                   SYMB0950
L4:           BINX ~ W.[30:6] ;  D1 ;                                   SYMB0960
L5:           BINX ~ W.[36:6] ;                                         SYMB0970
LOADB:        OSC ~ B[BINX].[33:6] ;  AINX ~ B[BINX].[39:9] ;           SYMB0980
              LP  ~ TRUE ;                                              SYMB0990
              FOR I ~ 1 STEP 1 UNTIL OSC DO                             SYMB1000
         BEGIN                                                          SYMB1010
              IF I{15 THEN                                              SYMB1020
         BEGIN                                                          SYMB1030
              IF I ! 1 THEN GO TO SKIP ;                                SYMB1040
              OSTS ~ A[AINX].[9:39] ;  OSTS1 ~ A[AINX].[3:6]            SYMB1050
         END                                                            SYMB1060
              ELSE IF I{30 THEN                                         SYMB1070
         BEGIN                                                          SYMB1080
              IF I ! 16 THEN GO TO SKIP ;                               SYMB1090
              OSTS ~ A[AINX+1].[9:39] ;  OSTS1 ~ A[AINX+1].[3:6]        SYMB1100
         END                                                            SYMB1110
              ELSE                                                      SYMB1120
         BEGIN                                                          SYMB1130
              IF I ! 31 THEN GO TO SKIP ;                               SYMB1140
              OSTS ~ A[AINX+2].[9:39] ;  OSTS1 ~ A[AINX+2].[3:6]        SYMB1150
         END ;                                                          SYMB1160
SKIP:         GO TO SW1[IF I{15 THEN I ELSE IF I{30 THEN I-15           SYMB1170
              ELSE I-30] ;                                              SYMB1180
S1:           MOVE ~ OSTS1.[42:3] ;  D2;                                SYMB1190
S2:           MOVE ~ OSTS1.[45:3] ;  D2;                                SYMB1200
S3:           MOVE ~ OSTS.[9:3] ;   D2 ;                                SYMB1210
S4:           MOVE ~ OSTS.[12:3] ;  D2 ;                                SYMB1220
S5:           MOVE ~ OSTS.[15:3] ;  D2 ;                                SYMB1230
S6:           MOVE ~ OSTS.[18:3] ;  D2 ;                                SYMB1240
S7:           MOVE ~ OSTS.[21:3] ;  D2 ;                                SYMB1250
S8:           MOVE ~ OSTS.[24:3] ;  D2 ;                                SYMB1260
S9:           MOVE ~ OSTS.[27:3] ;  D2 ;                                SYMB1270
S10:          MOVE ~ OSTS.[30:3] ;  D2 ;                                SYMB1280
S11:          MOVE ~ OSTS.[33:3] ;  D2 ;                                SYMB1290
S12:          MOVE ~ OSTS.[36:3] ;  D2 ;                                SYMB1300
S13:          MOVE ~ OSTS.[39:3] ;  D2 ;                                SYMB1310
S14:          MOVE ~ OSTS.[42:3] ;  D2 ;                                SYMB1320
S15:          MOVE ~ OSTS.[45:3] ;  D2 ;                                SYMB1330
M1:           IF NOT BOOLEAN(I) THEN GO TO Y1 ;                         SYMB1340
              XN ~ X ;  YN ~ Y ;                                        SYMB1350
              IF MOVE = 7 THEN                                          SYMB1360
         BEGIN                                                          SYMB1370
              LP ~ TRUE ;  I ~ I+1 ;  GO TO EL                          SYMB1380
         END ;                                                          SYMB1390
              XN ~ XN + MOVE|XA ;  YN ~ YN + MOVE|YA ;  GO TO EL ;      SYMB1400
Y1:           XN ~ XN - YA|MOVE ;  YN ~ YN + XA|MOVE ;                  SYMB1410
              PLOT(XN, YN, IF NOT LP THEN 2 ELSE 3) ;  LP ~ FALSE ;     SYMB1420
EL:      END ;                                                          SYMB1430
              IF N < 0 THEN                                             SYMB1440
         BEGIN                                                          SYMB1450
              PLOT(X0,Y0,3)  ;  GO TO EXIT  ;                           SYMB1460
         END ;                                                          SYMB1470
              X ~ X +6|XA ;  Y ~ Y + 6|YA                               SYMB1480
         END ;                                                          SYMB1490
EXIT:    END SYMBOL PROCEDURE  ;                                        SYMB1500
                                                                        MAIN0360
PROCEDURE     NUMBER(X, Y, HGT, FLT, THETA, N) ;                        NUMB0010
                                                                        NUMB0020
VALUE         X, Y, HGT, FLT, THETA, N ;                                NUMB0030
INTEGER       N ;                                                       NUMB0040
REAL          X, Y, HGT, FLT, THETA ;                                   NUMB0050
                                                                        NUMB0060
         BEGIN                                                          NUMB0070
                                                                        NUMB0080
INTEGER       INT, A1, FRAC, DF, A2 ;                                   NUMB0090
BOOLEAN       B  ;                                                      NUMB0100
LABEL         SIGN1,SIGN2,SIGN3,SIGN4,SIGN5,FR,FR1,ERROR,EXIT ;         NUMB0110
LABEL         JF1, JF2, JF3, JF4, JF5 ;                                 NUMB0120
SWITCH        JF ~ JF1, JF2, JF3, JF4, JF5 ;                            NUMB0130
DEFINE        D1=GO TO FR#,D2=GO TO FR1#,D3=GO TO ERROR#,D4=GO TO EXIT#;NUMB0140
ALPHA ARRAY   BCD[0:2] ;                                                NUMB0150
                                                                        NUMB0160
STREAM PROCEDURE CONVERT(XO, XA) ;                                      NUMB0170
         BEGIN                                                          NUMB0180
              SI ~ XO ;  DI ~ XA ;  DI ~ DI+2 ;  DS ~ 6DEC              NUMB0190
         END ;                                                          NUMB0200
                                                                        NUMB0210
              FLT ~ (SIGN(FLT) | 0.5 | 10*(-N)) + FLT  ;                NUMB0220
              IF ABS(INT ~ ENTIER(IF B ~ FLT } 0 THEN FLT               NUMB0230
              ELSE IF FLT MOD 1 ! 0 THEN FLT+1 ELSE FLT)) > 99999       NUMB0240
              THEN D3  ;                                                NUMB0250
              FRAC ~ ABS(INT)  ;  CONVERT(FRAC,A1)  ;                   NUMB0260
              IF    A1.[18:6] ! 0 THEN GO TO SIGN1 ;                    NUMB0270
              IF    A1.[24:6] ! 0 THEN GO TO SIGN2 ;                    NUMB0280
              IF    A1.[30:6] ! 0 THEN GO TO SIGN3 ;                    NUMB0290
              IF    A1.[36:6] ! 0 THEN GO TO SIGN4 ;                    NUMB0300
              IF    A1.[42:6] ! 0 THEN GO TO SIGN5 ;                    NUMB0310
              A1 ~ IF B THEN A1+52357696512 ELSE A1+52357696256 ;  D1 ; NUMB0320
SIGN1:        A1 ~ IF B THEN A1+51539607552 ELSE A1+47244640256 ;  D1 ; NUMB0330
SIGN2:        A1 ~ IF B THEN A1+52344913920 ELSE A1+52277805056 ;  D1 ; NUMB0340
SIGN3:        A1 ~ IF B THEN A1+52357496832 ELSE A1+52356448256 ;  D1 ; NUMB0350
SIGN4:        A1 ~ IF B THEN A1+52357693440 ELSE A1+52357677056 ;  D1 ; NUMB0360
SIGN5:        A1 ~ IF B THEN A1+52357696512 ELSE A1+52357696256 ;       NUMB0370
FR:           BCD[0] ~ A1 ;                                             NUMB0380
              IF N {0 THEN                                              NUMB0390
         BEGIN                                                          NUMB0400
              SYMBOL(X, Y, HGT, BCD, THETA, 6) ;  D4                    NUMB0410
         END ;                                                          NUMB0420
              IF N > 5 THEN D3  ;                                       NUMB0430
              FRAC ~ ENTIER((ABS(FLT) - ABS(INT)) | 10*N)  ;            NUMB0440
              CONVERT(FRAC, DF) ;                 A2 ~ 27917287424 ;    NUMB0450
              GO TO JF[N] ;                                             NUMB0460
JF1:          A2.[18:6] ~ DF.[42:6] ;  D2 ;                             NUMB0470
JF2:          A2.[18:12] ~ DF.[36:12] ;  D2 ;                           NUMB0480
JF3:          A2.[18:18] ~ DF.[30:18] ;  D2 ;                           NUMB0490
JF4:          A2.[18:24] ~  DF.[24:24] ;  D2 ;                          NUMB0500
JF5:          A2.[18:30] ~ DF.[18:30] ;                                 NUMB0510
FR1:          BCD[1] ~ A2 ;                                             NUMB0520
              SYMBOL(X, Y, HGT, BCD, THETA, N+7) ;  D4 ;                NUMB0530
ERROR:        BCD[0] ~ "ILLEGA" ;  BCD[1]~"L NUMB" ;  BCD[2]~ "ER    " ;NUMB0540
                   SYMBOL(X, Y, HGT, BCD, THETA, 14) ;                  NUMB0550
EXIT:    END NUMBER PROCEDURE  ;                                        NUMB0560
                                                                        MAIN0370
PROCEDURE     AXIS(X,Y,BCD,NC,SIZE,THETA,YMIN,DY) ;                     AXIS0010
                                                                        AXIS0015
VALUE         X,Y,NC,SIZE,THETA,YMIN,DY ;                               AXIS0020
REAL          X, Y, SIZE, THETA, YMIN, DY ;                             AXIS0030
INTEGER       NC  ;                                                     AXIS0040
ALPHA ARRAY   BCD[0] ;                                                  AXIS0050
                                                                        AXIS0060
         BEGIN                                                          AXIS0070
                                                                        AXIS0080
REAL          SGN,TH,CTH,STH,XB,YB,XA,YA,XC,YC,CHAR,ABSV,EXPP,ADY,TNC  ;AXIS0090
INTEGER       N, I, NT, NAC ;      ALPHA ARRAY ABCD[0:1] ;              AXIS0100
LABEL         L89,L90,L50  ;                                            AXIS0110
                                                                        AXIS0120
              SGN ~ IF NC = 0 THEN 1 ELSE SIGN(NC)  ;                   AXIS0130
              NAC ~ ABS(NC)  ;                                          AXIS0140
              TH ~ THETA | 0.017455  ;  N ~ SIZE  ;  CTH ~ COS(TH)  ;   AXIS0150
              STH ~ SIN(TH)  ;  XB ~ X  ;  YB ~ Y  ;                    AXIS0160
              XA ~ X - 0.1 | SGN | STH  ;  YA ~ Y + 0.1 | SGN | CTH  ;  AXIS0170
              PLOT(XA, YA, 3) ;                                         AXIS0180
              FOR I ~ 1 STEP 1 UNTIL N DO                               AXIS0190
         BEGIN                                                          AXIS0200
              PLOT(XB, YB, 2) ;  XC ~ XB + CTH ;  YC ~ YB + STH ;       AXIS0210
              PLOT(XC, YC, 2) ;  XA ~ XA + CTH ;  YA ~ YA + STH ;       AXIS0220
              PLOT(XA, YA, 2) ;  XB ~ XC ;  YB ~ YC                     AXIS0230
         END ;                                                          AXIS0240
              IF NC = 0 THEN GO TO L50  ;                               AXIS0250
              CHAR ~ ABS(YMIN)  ;  ABSV ~ ABS(N|DY + YMIN)  ;           AXIS0260
              IF ABSV < CHAR THEN ABSV ~ CHAR  ;  EXPP ~ 0  ;           AXIS0270
              ADY ~ DY  ;                                               AXIS0280
L89:          IF ABS(ADY) < 0.001 THEN                                  AXIS0290
         BEGIN                                                          AXIS0300
              IF ABSV > 999.9 THEN GO TO L90  ;                         AXIS0310
              ADY ~ ADY | 10.0  ;  ABSV ~ ABSV | 10.0  ;                AXIS0320
              EXPP ~ EXPP - 1  ;  GO TO L89  ;                          AXIS0330
         END ;                                                          AXIS0340
              IF ABSV } 9999.0 THEN                                     AXIS0350
         BEGIN                                                          AXIS0360
              ADY ~ ADY | 0.1  ;  ABSV ~ ABSV | 0.1  ;                  AXIS0370
              EXPP ~ EXPP + 1  ;  GO TO L89  ;                          AXIS0380
         END ;                                                          AXIS0390
L90:          ABSV ~ YMIN | 10*(-EXPP) + N | ADY  ;                     AXIS0400
              XC ~ 0.2 | SGN - 0.05  ;                                  AXIS0410
              XA ~ XB - XC | STH - 0.53 | CTH  ;                        AXIS0420
              YA ~ YB + XC | CTH - 0.53 | STH  ;  N ~ N + 1  ;          AXIS0430
              FOR I ~ 1 STEP 1 UNTIL N DO                               AXIS0440
         BEGIN                                                          AXIS0450
              IF ABS(ABSV) { 0.0009 THEN ABSV ~ 0  ;                    AXIS0460
              NUMBER(XA, YA, .1, ABSV, THETA, 3) ;  ABSV ~ ABSV - ADY ; AXIS0470
              XA ~ XA - CTH ;  YA ~ YA - STH                            AXIS0480
         END ;                                                          AXIS0490
              TNC ~ NAC + 7 ;                                           AXIS0500
              XC ~ SIZE | 0.5 - 0.06 | TNC  ;                           AXIS0510
              YC ~ 0.07 - SGN | 0.4  ;                                  AXIS0520
              XA ~ X + XC | CTH + YC | STH  ;                           AXIS0530
              YA ~ Y + XC | STH - YC | CTH  ;                           AXIS0540
              SYMBOL(XA, YA, .14, BCD, THETA, NAC) ;                    AXIS0550
              IF EXPP = 0 THEN GO TO L50  ;                             AXIS0560
              XC ~ (TNC - 6) | 0.12  ;                                  AXIS0570
              XA ~ XA + XC | CTH  ;                                     AXIS0580
              YA ~ YA + XC | STH  ;                                     AXIS0590
              ABCD[0] ~ "(10  )"  ;                                     AXIS0600
              SYMBOL(XA, YA, .14, ABCD, THETA, 6) ;                     AXIS0610
              XA ~ XA + 0.18 | CTH - 0.07 | STH  ;                      AXIS0620
              YA ~ YA + 0.18 | STH + 0.07 | CTH  ;                      AXIS0630
              NUMBER(XA, YA,.07, EXPP, THETA, 0 ) ;                     AXIS0640
L50:     END AXIS PROCEDURE  ;                                          AXIS0650
                                                                        MAIN0380
PROCEDURE     DXDY(YMAX,YMIN,TDY)  ;                                    DXDY0010
                                                                        DXDY0015
VALUE         YMAX  ;                                                   DXDY0020
REAL          YMAX,YMIN,TDY  ;                                          DXDY0030
                                                                        DXDY0040
         BEGIN                                                          DXDY0050
                                                                        DXDY0060
REAL          ADY,K1,V  ;                                               DXDY0070
INTEGER       K  ;                                                      DXDY0080
LABEL         L1,L2,L3,L4,FIN  ;                                        DXDY0090
                                                                        DXDY0100
              ADY ~ YMAX - YMIN  ;                                      DXDY0110
              IF YMIN ! 0 THEN                                          DXDY0120
         BEGIN                                                          DXDY0130
              K ~ -ENTIER(LN(ADY)|.4342944819)  ;                       DXDY0140
              K1 ~ 10*K  ;                                              DXDY0150
              K ~ ENTIER (YMIN|K1)  ;                                   DXDY0160
              YMIN ~ K/K1                                               DXDY0170
         END ;                                                          DXDY0180
              ADY ~ YMAX - YMIN  ;  K1 ~ 0  ;                           DXDY0190
L1:           IF ADY { 10.5 THEN GO TO L3 ELSE GO TO L4  ;              DXDY0200
L2:           ADY ~ ADY | 10.0  ;                                       DXDY0210
              K1 ~ K1 - 1  ;  GO TO L1  ;                               DXDY0220
L3:           IF ADY { 8.4 THEN GO TO L2  ;                             DXDY0230
              TDY ~ 10.0  ;  GO TO FIN  ;                               DXDY0240
L4:           IF ADY { 21.0 THEN                                        DXDY0250
         BEGIN                                                          DXDY0260
              TDY ~ 20.0  ;  GO TO FIN  ;                               DXDY0270
         END ;                                                          DXDY0280
              IF ADY { 42.0 THEN                                        DXDY0290
         BEGIN                                                          DXDY0300
              TDY ~ 40.0  ;  GO TO FIN  ;                               DXDY0310
         END ;                                                          DXDY0320
              IF ADY { 52.5 THEN                                        DXDY0330
         BEGIN                                                          DXDY0340
              TDY ~ 50.0  ;  GO TO FIN  ;                               DXDY0350
         END ;                                                          DXDY0360
              IF ADY > 84.0 THEN                                        DXDY0370
         BEGIN                                                          DXDY0380
              ADY ~ ADY/10.0  ;                                         DXDY0390
              K1 ~ K1 + 1  ;  GO TO L1  ;                               DXDY0400
         END ;                                                          DXDY0410
              TDY ~ 80.0  ;                                             DXDY0420
FIN:          TDY ~ TDY | 10.0*(K1)  ;                                  DXDY0430
         END DXDY PROCEDURE  ;                                          DXDY0440
                                                                        MAIN0390
PROCEDURE     SCALES (X,N,XMIN,DX,K)  ;                                 SCLS0010
                                                                        SCLS0020
VALUE         N,XMIN,DX,K  ;                                            SCLS0030
REAL          XMIN,DX  ;                                                SCLS0040
INTEGER       N,K  ;                                                    SCLS0050
REAL ARRAY    X[0]  ;                                                   SCLS0060
                                                                        SCLS0070
         BEGIN                                                          SCLS0080
                                                                        SCLS0090
INTEGER       I,NP  ;                                                   SCLS0100
                                                                        SCLS0110
              NP ~ N|K - (K - 1)  ;                                     SCLS0120
              FOR I ~ 1 STEP K UNTIL NP DO                              SCLS0130
              X[I] ~ (X[I] -XMIN)/DX  ;                                 SCLS0140
         END SCALES PROCEDURE  ;                                        SCLS0150
                                                                        MAIN0400
PROCEDURE     SCALE(X,N,S,YMIN,DY,K)  ;                                 SCLE0010
                                                                        SCLE0015
VALUE         N,S,K  ;                                                  SCLE0020
INTEGER       N,K  ;                                                    SCLE0030
REAL          S,YMIN,DY  ;                                              SCLE0040
REAL ARRAY    X[0]  ;                                                   SCLE0050
                                                                        SCLE0060
         BEGIN                                                          SCLE0070
                                                                        SCLE0080
REAL          YMAX  ;                                                   SCLE0090
INTEGER       I,NP  ;                                                   SCLE0100
                                                                        SCLE0110
              NP ~ N | K - (K - 1)  ;                                   SCLE0120
              YMAX ~ YMIN ~ X[1]  ;                                     SCLE0130
              FOR I ~ K+1 STEP K UNTIL NP DO                            SCLE0140
         BEGIN                                                          SCLE0150
              IF YMAX < X[I] THEN YMAX ~ X[I] ELSE                      SCLE0160
              IF X[I] < YMIN THEN YMIN ~ X[I]  ;                        SCLE0170
         END ;                                                          SCLE0180
              YMAX ~ (YMAX - YMIN)/S + YMIN  ;                          SCLE0190
              DXDY(YMAX,YMIN,DY)  ;                                     SCLE0200
              FOR I ~ 1 STEP K UNTIL NP DO                              SCLE0210
              X[I] ~ (X[I] - YMIN)/DY  ;                                SCLE0220
         END SCALE  PROCEDURE  ;                                        SCLE0230
                                                                        MAIN0410
PROCEDURE     LINE(X, Y, N, K) ;                                        LINE0010
                                                                        LINE0020
VALUE         N, K ;                                                    LINE0030
INTEGER       N,K  ;                                                    LINE0040
ARRAY         X,Y[0]  ;                                                 LINE0050
                                                                        LINE0060
         BEGIN                                                          LINE0070
                                                                        LINE0080
REAL          XX,YY,DF,DL  ;                                            LINE0090
INTEGER       I, I3, NP ;                                               LINE0100
                                                                        LINE0110
              I3 ~ 3 ;  NP ~ N|K - (K- 1)  ;                            LINE0120
              PLOT(XX,YY,4)  ;                                          LINE0130
              DF ~ SQRT((X[1] - XX)*2 + (Y[1] - YY)*2)  ;               LINE0140
              DL ~ SQRT((X[NP] - XX)*2 + (Y[NP] - YY)*2)  ;             LINE0150
              IF DF < DL THEN                                           LINE0160
         BEGIN                                                          LINE0170
              FOR I ~ 1 STEP K UNTIL NP DO                              LINE0180
         BEGIN                                                          LINE0190
              PLOT(X[I], Y[I], I3) ;  I3 ~ 2                            LINE0200
         END                                                            LINE0210
         END                                                            LINE0220
              ELSE                                                      LINE0230
         BEGIN                                                          LINE0240
              FOR I ~ NP STEP -K UNTIL 1 DO                             LINE0250
         BEGIN                                                          LINE0260
              PLOT(X[I],Y[I],I3)  ;  I3 ~ 2  ;                          LINE0270
         END ;                                                          LINE0280
         END ;                                                          LINE0290
         END LINE PROCEDURE  ;                                          LINE0300
                                                                        MAIN0420
PROCEDURE     DASHLINE(X,Y,N1,K)  ;                                     DHLN0010
                                                                        DHLN0020
VALUE         N1,K  ;                                                   DHLN0030
INTEGER       N1,K  ;                                                   DHLN0040
REAL ARRAY    X,Y[0]  ;                                                 DHLN0050
                                                                        DHLN0060
         BEGIN                                                          DHLN0070
                                                                        DHLN0080
INTEGER       I,NP,M,N  ;                                               DHLN0090
REAL          PEN,XN,YN,ADX,ADY,DX,DY,DLTX,DLTY  ;                      DHLN0100
LABEL         L1  ;                                                     DHLN0110
                                                                        DHLN0120
              PEN ~ 2  ;  M ~ 10  ;  NP ~ N1 | K - (K - 1)  ;           DHLN0130
              XN ~ X[1]  ;  YN ~ Y[1]  ;                                DHLN0140
              PLOT(XN,YN,3)  ;                                          DHLN0150
              FOR I ~ K+1 STEP K UNTIL NP DO                            DHLN0160
         BEGIN                                                          DHLN0170
              DX ~ X[I] - XN  ;                                         DHLN0180
              DY ~ Y[I] - YN  ;                                         DHLN0190
              ADX ~ ABS(DX)  ;  ADY ~ ABS(DY)  ;                        DHLN0200
              IF ADX > ADY THEN                                         DHLN0210
         BEGIN                                                          DHLN0220
              DLTX ~ SIGN(DX) | 0.01  ;                                 DHLN0230
              DLTY ~ 0.01 | DY/ADX  ;                                   DHLN0240
              N ~ ADX | 100.0  ;                                        DHLN0250
         END                                                            DHLN0260
              ELSE                                                      DHLN0270
         BEGIN                                                          DHLN0280
              DLTY ~ SIGN(DY) | 0.01  ;                                 DHLN0290
              DLTX ~ 0.01 | DX / ADY  ;                                 DHLN0300
              N ~ ADY | 100.0                                           DHLN0310
         END ;                                                          DHLN0320
L1:           IF M { N THEN                                             DHLN0330
         BEGIN                                                          DHLN0340
              PLOT(XN ~ XN + M | DLTX,YN ~ YN + M | DLTY,PEN)  ;        DHLN0350
              N ~ N - M  ;                                              DHLN0360
              IF PEN = 3 THEN                                           DHLN0370
         BEGIN                                                          DHLN0380
              M ~ 10  ;  PEN ~ 2  ;                                     DHLN0390
         END                                                            DHLN0400
              ELSE                                                      DHLN0410
         BEGIN                                                          DHLN0420
              M ~  5  ;  PEN ~ 3  ;                                     DHLN0430
         END ;                                                          DHLN0440
              GO TO L1  ;                                               DHLN0450
         END                                                            DHLN0460
              ELSE                                                      DHLN0470
         BEGIN                                                          DHLN0480
              PLOT(XN ~ XN + N | DLTX,YN ~ YN + N | DLTY,PEN)  ;        DHLN0490
              M ~ M - N  ;                                              DHLN0500
         END ;                                                          DHLN0510
         END ;                                                          DHLN0520
              PLOT(XN,YN,2)  ;                                          DHLN0530
         END DASHLINE PROCEDURE  ;                                      DHLN0540
                                                                        MAIN0430
PROCEDURE     NAMELINE(X,Y,N,K,A,T,DASH)  ;                             NMLN0010
                                                                        NMLN0020
VALUE N,K,T  ;  INTEGER N,K,T  ;                                        NMLN0030
REAL ARRAY X,Y[0]  ;                                                    NMLN0040
ALPHA ARRAY A[0]  ;                                                     NMLN0050
BOOLEAN DASH  ;                                                         NMLN0060
                                                                        NMLN0070
         BEGIN                                                          NMLN0080
                                                                        NMLN0090
INTEGER       T1,N1,I,J,NP  ;                                           NMLN0100
REAL          TH,XM,YM,MX,DX,DY,S,YL  ;                                 NMLN0110
REAL ARRAY    X1,Y1[0:N DIV 2 + 2]  ;                                   NMLN0120
                                                                        NMLN0130
              N1 ~ N DIV 2  ;                                           NMLN0140
              NP ~ N | K - (K - 1)  ;                                   NMLN0150
              I ~ (N1 - 1) | K + 1  ;                                   NMLN0160
              IF DASH THEN                                              NMLN0170
              DASHLINE(X,Y,I,K)                                         NMLN0180
              ELSE                                                      NMLN0190
              LINE(X,Y,I,K)  ;                                          NMLN0200
              DX ~ X[I + K] - X[I - K]  ;                               NMLN0210
              DY ~ Y[I + K] - Y[I - K]  ;                               NMLN0220
              IF DX = 0 THEN                                            NMLN0230
              TH ~ 1.5707963                                            NMLN0240
              ELSE                                                      NMLN0250
              TH ~ ARCTAN(DY/DX)  ;                                     NMLN0260
              IF T } 0 THEN                                             NMLN0270
         BEGIN                                                          NMLN0280
              T1 ~ T DIV 2  ;                                           NMLN0290
              YL ~ (X[I] | (Y[I - K] - Y[I + K]) + X[I - K] | Y[I + K]  NMLN0300
              - X[I + K] | Y[I - K])/(X[I - K] - X[I + K])  ;           NMLN0310
              IF YL < Y[I] THEN                                         NMLN0320
              S ~ -1                                                    NMLN0330
              ELSE                                                      NMLN0340
              S ~ 1  ;                                                  NMLN0350
              S ~ 0.05 + S | 0.09  ;                                    NMLN0360
              XM ~ S | SIN(TH) - 0.0857 | T1 | COS(TH) + X[I]  ;        NMLN0370
              YM ~ -S | COS(TH)-0.0857 | T1 | SIN(TH) + Y[I]  ;         NMLN0380
              TH ~ 57.2959125 | TH  ;                                   NMLN0390
              SYMBOL(XM,YM,0.10,A,TH,T)  ;                              NMLN0400
         END                                                            NMLN0410
              ELSE                                                      NMLN0420
              SYMBOL(X[I],Y[I],0.30,A,TH,T)  ;                          NMLN0430
              J ~ 0  ;                                                  NMLN0440
              FOR I ~ I     STEP K UNTIL NP DO                          NMLN0450
         BEGIN                                                          NMLN0460
              J ~ J + 1  ;                                              NMLN0470
              X1[J] ~ X[I]  ;  Y1[J] ~ Y[I]  ;                          NMLN0480
         END ;                                                          NMLN0490
              IF DASH THEN                                              NMLN0500
              DASHLINE(X1,Y1,J,K)                                       NMLN0510
              ELSE                                                      NMLN0520
              LINE(X1,Y1,J,K)  ;                                        NMLN0530
         END NAMELINE PROCEDURE  ;                                      NMLN0540

L140:    END.
$ CARD LIST
