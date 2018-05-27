COMMENT       BIVARIATE CORRELATION AND REGRESSION ANALYSIS.  THE LONG  00000100
              METHOD OF DIFFERENCES FROM THE AVERAGE IS USED            00000200
              RATHER THAN THE SHORT-CUT SUM OF SQUARES.  BOTH           00000300
              Y = AX + B AND X = A"Y + B" ARE FITTED.  THE LINE MAY BE  00000400
              FORCED THROUGH THE ORIGIN .                               00000500
               MODIFIED FOR TELETYPE JULY 1967 F. LUCAS        ;        00000600
         BEGIN                                                          00000700
COMMENT        CUBE LIBRARY NUMBER IS G200010.  THE PROGRAM NAME IS     00000800
               "B0301/TTY".  THIS VERSION DATED 5/3/68;                 00000900
INTEGER       J, N, NAME ;                                              00001000
                                                                        00001100
ALPHA          MORDAT;                                                  00001200
BOOLEAN       S, SWF, SWP ;                                             00001300
                                                                        00001400
REAL          IJ, DJ, ESTIMATE, RESIDUAL, SUMX, SUMY, SUMXY, SUMX2,     00001500
              SUMY2, DXY, DX2, DY2, AVERAGEX, AVERAGEY, DIFFX, DIFFY,   00001600
              VARIANCEX, VARIANCEY, SE, SA, SB, TA, TB, R2,             00001700
              R, A, B, VARIANCE ;                                       00001800
FORMAT         CARD2(2F12.5),                                           00001900
              CARD3 (X10, 4I5) ;                                        00002000
LIST          SIZES (SWF, SWP, NAME, N) ;                               00002100
FILE IN        FID DISK SERIAL "AAAAAAA" "BBBBBBB" (2,10,30);           00002200
ALPHA FILE IN TTIN 14 (2,8);                                            00002300
ALPHA FILE OUT TOUT 14 (2,8);                                           00002400
FORMAT IN      F1(X8,I1),  F2(X8,I*),  F3(X8,A1,A6), F4(X8,A3);         00002500
ARRAY          TZ[0:7];                                                 00002600
INTEGER        STA,NC;                                                  00002700
ALPHA          AA,BB,AA1,BB1,ID1,ID2;                                   00002800
FORMAT OUT     HEAD(X8,"{!!!SIMPLE REGRESSION ANALYSIS{!!~"),           00002900
               FORM1(X8,"CASE NO.",I11,"{!!~"),                         00003000
               FORM2(X8,"DATA POINTS",I8 ,"{!!~"),                      00003100
               FORM3(X8,"CONSTANT TERM{!!~"),                           00003200
            AVERAGEF(X8,"{!AVERAGE X",E14.6,X5,"AVERAGE Y",E14.6,"{!~"),00003300
               VARIANCEF(X8,"VARIANCE X",E14.6,X4,"VARIANCE Y",         00003400
                   E14.6,"{!~"),                                        00003500
               RF1(X8,"CORRELATION COEFFICIENT",E14.6,"{!~"),           00003600
               RF2(X8,"FRACTION OF VARIANCE REMOVED",E14.6,"{!~"),      00003700
               HEAD1(X8,"{!Y = AX + B{!!~"),                            00003800
               AF1(X8,"SLOPE",E14.6,"{!~"),                             00003900
               AF2(X8,"SLOPE ERROR",E14.6,"{!~"),                       00004000
               AF3(X8,"SLOPE T-VALUE",E14.6,"{!~"),                     00004100
               BF1(X8,"INTERCEPT",E14.6,"{!~"),                         00004200
               BF2(X8,"INTERCEPT ERROR",E14.6,"{!~"),                   00004300
               BF3(X8,"INTERCEPT T-VALUE",E14.6,"{!~"),                 00004400
               ERRORF1(X8,"VARIANCE OF FIT",E14.6,"{!~"),               00004500
               ERRORF2(X8,"STANDARD ERROR OF ESTIMATE",E14.6,"{!!~"),   00004600
               HEAD2(X8,"{!X = AY + B{!!~"),                            00004700
               RESIDUALS(X8," INDEPENDENT    DEPENDENT     ESTIMATE",   00004800
                     "      RESIDUALS{!!~"),                            00004900
               RESF(X8,3(E13.5,X1),E13.5"~"),                           00005000
               FCRLF(X8,"{!~"),                                         00005100
               FORCE(X8,"{!NO CONSTANT TERM{!~"),                       00005200
               TITLE1(X8,"{!!Y = AX{!!~"),                              00005300
               TITLE2(X8,"{!!X = AY{!!~"),                              00005400
               FNOF1(X8,"{!YOUR FILE NOT FOUND. TRY AGAIN.{!!~"),       00005500
               FSWF(X8,"{!TO FORCE LINE THRU ORIGIN TYPE 1 ELSE 0 ~"),  00005600
               FSWP(X8,"{!TO PRINT DIFFERENCES TYPE 1 ELSE 0 ~"),       00005700
               FNAME(X8,"{!ENTER CASE NO. ~"),                          00005800
               FSTR(X8,"{!!BEGINNING OF SIMPLE REGRESSION ANALYSIS{!~"),00005900
               FSTT(X8,"ENTER YOUR SETUP VALUES AS ASKED FOR{!!~"),     00006000
               FDPT(X8,"{!NO. OF DATA POINTS = ~"),                     00006100
               FMOR(X8,"{!TO DO ANOTHER SET TYPE YES {!~"),             00006200
               FPGM(X8,"{!ENTER YOUR DATA NAMES{!~"),                   00006300
               FSX(X8,"{!SUFFIX = ~"),                                  00006400
               FPX(X8,"PREFIX = ~");                                    00006500
LIST          CASE (NAME, N),                                           00006600
              AVERAGEO (AVERAGEX, AVERAGEY),                            00006700
              VARIANCEO (VARIANCEX, VARIANCEY),                         00006800
              RO (R, R | R),                                            00006900
              AO (A, SA, TA),                                           00007000
              BO (B, SB, TB),                                           00007100
              ERRORO (VARIANCE, SE),                                    00007200
              RESO (IJ, DJ, ESTIMATE, RESIDUAL),                        00007300
              RFOO (R, R2) ;                                            00007400
LABEL         START,EXIT;                                               00007500
DEFINE         WT=WRITE(TOUT(STA)#, RT=READ(TTIN(STA)#,PAPER=TOUT(STA)#;00007600
PROCEDURE      BLANKIT(XX);                                             00007700
ALPHA          XX;                                                      00007800
          BEGIN                                                         00007900
               IF XX.[12:6]="~" THEN XX.[12:36]~"      " ELSE           00008000
               IF XX.[18:6]="~" THEN XX.[18:30]~"     " ELSE            00008100
               IF XX.[24:6]="~" THEN XX.[24:24]~"    " ELSE             00008200
               IF XX.[30:6]="~" THEN XX.[30:18]~"   " ELSE              00008300
               IF XX.[36:6]="~" THEN XX.[36:12]~"  " ELSE               00008400
               IF XX.[42:6]="~" THEN XX.[42:06]~" " ELSE                00008500
          END OF BLANKIT;                                               00008600
INTEGER STREAM PROCEDURE FINDGP(B);                                     00008700
          BEGIN                                                         00008800
LABEL          HERE;                                                    00008900
               SI~B; 40(IF SC="~" THEN JUMP OUT 1 TO HERE;              00009000
               SI~SI+1; TALLY ~TALLY+1);                                00009100
HERE:          FINDGP~TALLY;                                            00009200
          END OF FINDGP;                                                00009300
               STA~STATUS(TZ[*]);                                       00009400
               STA.[9:9]~TZ[0].[9:9]; WT,FSTR);                         00009500
               WT,FSTT);                                                00009600
START:         WT,FSWF);  RT,F1,SWF);                                   00009700
               WT,FSWP);  RT,F1,SWP);                                   00009800
               WT,FNAME);  RT,4,TZ[*]);                                 00009900
               NC~FINDGP(TZ[1]);                                        00010000
               READ(TZ[*],F2,NC,NAME);                                  00010100
               WT,FDPT);    RT,4,TZ[*]);                                00010200
               NC~FINDGP(TZ[1]);                                        00010300
               READ(TZ[*],F2,NC,N);                                     00010400
              SUMX ~ SUMY ~ SUMXY ~ SUMX2 ~ SUMY2 ~                     00010500
              DXY ~ DX2 ~ DY2 ~ 0 ;                                     00010600
         BEGIN                                                          00010700
REAL ARRAY    X[0:N], Y[0:N] ;                                          00010800
LABEL          LOVR,BYP;                                                00010900
COMMENT       THE PROCEDURE PRINTDIFF COMPUTES THE RESIDUALS AND PRINTS 00011000
              THEM ALONG WITH THE CORRESPONDING DATA ;                  00011100
PROCEDURE     PRINTDIFF ;                                               00011200
         BEGIN                                                          00011300
              WRITE (PAPER, RESIDUALS) ;                                00011400
              FOR J ~ 1 STEP 1 UNTIL N DO                               00011500
         BEGIN                                                          00011600
              IF S THEN                                                 00011700
         BEGIN                                                          00011800
              IJ ~ X[J] ;         DJ ~ Y[J]                             00011900
         END                                                            00012000
              ELSE                                                      00012100
         BEGIN                                                          00012200
              IJ ~ Y[J] ;         DJ ~ X[J]                             00012300
         END ;                                                          00012400
              ESTIMATE ~ A | IJ + B ;                                   00012500
              RESIDUAL ~ DJ - ESTIMATE ;                                00012600
              WRITE (PAPER, RESF, RESO);                                00012700
               WT,FCRLF);                                               00012800
         END ;                                                          00012900
         END PRINTDIFF ;                                                00013000
LIST          DATA (X[J], Y[J]) ;                                       00013100
LABEL         FORCELINE ;                                               00013200
LOVR:          WT,FPGM); WT,FPX); RT,F3,AA,AA1);                        00013300
               ID1.[6:6]~AA;  ID1.[12:36]~AA1;                          00013400
               WT,FSX);  RT,F3,BB,BB1);                                 00013500
               ID2.[6:6]~BB;  ID2.[12:36]~BB1;                          00013600
               BLANKIT(ID1);   BLANKIT(ID2);                            00013700
               FILL FID WITH ID1,ID2;                                   00013800
               SEARCH(FID,TZ[*]);                                       00013900
               IF TZ[0]<2 THEN                                          00014000
          BEGIN                                                         00014100
               WT,FNOF1);  GO TO LOVR;                                  00014200
          END;                                                          00014300
              WRITE (PAPER, HEAD) ;                                     00014400
               WT,FORM1,NAME);  WT,FORM2,N);  WT,FORM3);                00014500
              FOR J ~ 1 STEP 1 UNTIL N DO                               00014600
         BEGIN                                                          00014700
              READ (FID  , CARD2, DATA) ;                               00014800
              IJ ~ X[J] ;         DJ ~ Y[J] ;                           00014900
              SUMX ~ IJ + SUMX ;                                        00015000
              SUMY ~ DJ + SUMY ;                                        00015100
              SUMXY ~ IJ | DJ + SUMXY ;                                 00015200
              SUMX2 ~ IJ * 2 + SUMX2 ;                                  00015300
              SUMY2 ~ DJ * 2 + SUMY2                                    00015400
         END ;                                                          00015500
              AVERAGEX ~ SUMX / N ;                                     00015600
              AVERAGEY ~ SUMY / N ;                                     00015700
              WRITE (PAPER, AVERAGEF, AVERAGEO) ;                       00015800
              FOR J ~ 1 STEP 1 UNTIL N DO                               00015900
         BEGIN                                                          00016000
              DIFFX ~ AVERAGEX - X[J] ;                                 00016100
              DIFFY ~ AVERAGEY - Y[J] ;                                 00016200
              DXY ~ DIFFX | DIFFY + DXY ;                               00016300
              DX2 ~ DIFFX * 2 + DX2 ;                                   00016400
              DY2 ~ DIFFY * 2 + DY2                                     00016500
         END ;                                                          00016600
              VARIANCEX ~ DX2 / (N-1) ;                                 00016700
              VARIANCEY ~ DY2 / (N-1) ;                                 00016800
              WRITE (PAPER, VARIANCEF, VARIANCEO) ;                     00016900
                                                                        00017000
COMMENT       COMPUTE THE CORRELATION COEFFICIENT ;                     00017100
              R ~ DXY / SQRT (DX2 | DY2) ;                              00017200
               WT,RF1,R);  WT,RF2,R|R);                                 00017300
COMMENT       COMPUTE REGRESSION COEFFICIENTS FOR Y = F(X) AND X = F(Y) 00017400
              ALONG WITH OTHER RELATED DATA ;                           00017500
              WRITE (PAPER, HEAD1) ;                                    00017600
              A ~ DXY / DX2 ;                                           00017700
              B ~ (SUMY - A | SUMX) / N ;                               00017800
              VARIANCE ~ (DY2 - A | DXY) / (N-2) ;                      00017900
              SE ~ SQRT (VARIANCE) ;                                    00018000
              SA ~ SE / SQRT (DX2) ;                                    00018100
              TA ~ A / SA ;                                             00018200
               WT,AF1,A); WT,AF2,SA); WT,AF3,TA);                       00018300
              SB ~ SE | SQRT (SUMX2 / (N | DX2)) ;                      00018400
              TB ~ B/SB ;                                               00018500
               WT,BF1,B); WT,BF2,SB); WT,BF3,TB);                       00018600
               WT,ERRORF1,VARIANCE); WT,ERRORF2,SE);                    00018700
              IF SWP THEN                                               00018800
         BEGIN                                                          00018900
              S ~ TRUE ;          PRINTDIFF                             00019000
         END ;                                                          00019100
                                                                        00019200
              WRITE (PAPER, HEAD2) ;                                    00019300
              A ~ DXY / DY2 ;                                           00019400
              B ~ (SUMX - A | SUMY) / N ;                               00019500
              VARIANCE ~ (DX2 - A | DXY) / (N-2) ;                      00019600
              SE ~ SQRT (VARIANCE) ;                                    00019700
              SA ~ SE / SQRT (DY2) ;                                    00019800
              TA ~ A / SA ;                                             00019900
               WT,AF1,A); WT,AF2,SA); WT,AF3,TA);                       00020000
              SB ~ SE | SQRT (SUMY2 / (N | DY2)) ;                      00020100
              TB ~ B / SB ;                                             00020200
               WT,BF1,B); WT,BF2,SB); WT,BF3,TB);                       00020300
               WT,ERRORF1,VARIANCE); WT,ERRORF2,SE);                    00020400
              IF SWP THEN                                               00020500
         BEGIN                                                          00020600
              S ~ FALSE ;          PRINTDIFF                            00020700
         END ;                                                          00020800
              IF NOT SWF THEN GO TO BYP;                                00020900
FORCELINE :   WRITE (PAPER, FORCE) ;                                    00021000
              B ~ R ~ 0 ;                                               00021100
              A ~ SUMXY / SUMX2 ;                                       00021200
              R2 ~ (A | SUMXY - SUMY | SUMY / N) / DY2 ;                00021300
              WRITE (PAPER, TITLE1) ;                                   00021400
               WT,RF1,R);  WT,RF2,R2);                                  00021500
              VARIANCE ~ (SUMY2 - A | SUMXY) / (N-1) ;                  00021600
              SE ~ SQRT (VARIANCE) ;                                    00021700
              SA ~ SE / SQRT (SUMX2) ;                                  00021800
              TA ~ A / SA ;                                             00021900
               WT,AF1,A); WT,AF2,SA); WT,AF3,TA);                       00022000
               WT,ERRORF1,VARIANCE); WT,ERRORF2,SE);                    00022100
              IF SWP THEN                                               00022200
         BEGIN                                                          00022300
              S ~ TRUE ;          PRINTDIFF                             00022400
         END ;                                                          00022500
              WRITE (PAPER, TITLE2) ;                                   00022600
              A ~ SUMXY / SUMY2 ;                                       00022700
              R2 ~ (A | SUMXY - SUMX | SUMX / N) / DX2 ;                00022800
               WT,RF1,R);  WT,RF2,R2);                                  00022900
               WT,AF1,A); WT,AF2,SA); WT,AF3,TA);                       00023000
               WT,ERRORF1,VARIANCE); WT,ERRORF2,SE);                    00023100
              IF SWP THEN                                               00023200
         BEGIN                                                          00023300
              S ~ FALSE ;          PRINTDIFF                            00023400
         END ;                                                          00023500
BYP:                                                                    00023600
                REWIND(FID);                                            00023700
               WT,FMOR);  RT,F4,MORDAT);                                00023800
               IF MORDAT = "YES" THEN GO TO START;                      00023900
         END ;                                                          00024000
EXIT:    END .                                                          00024100
