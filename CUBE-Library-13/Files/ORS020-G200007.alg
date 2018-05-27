 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          SRAN   1
COMMENT       BIVARIATE CORRELATION AND REGRESSION ANALYSIS.  THE LONG  SRAN   2
              METHOD OF DIFFERENCES FROM THE AVERAGE IS USED            SRAN   3
              RATHER THAN THE SHORT-CUT SUM OF SQUARES.  BOTH           SRAN   4
              Y = AX + B AND X = A"Y + B" ARE FITTED.  THE LINE MAY BE  SRAN   5
              FORCED THROUGH THE ORIGIN ;                               SRAN   6


         BEGIN                                                          SRAN   7
INTEGER       J, N, NAME ;                                              SRAN   8

BOOLEAN       S, SWF, SWP ;                                             SRAN   9

REAL          IJ, DJ, ESTIMATE, RESIDUAL, SUMX, SUMY, SUMXY, SUMX2,     SRAN  10
              SUMY2, DXY, DX2, DY2, AVERAGEX, AVERAGEY, DIFFX, DIFFY,   SRAN  11
              VARIANCEX, VARIANCEY, SE, SA, SB, TA, TB, R2,             SRAN  12
              R, A, B, VARIANCE ;                                       SRAN  13

FILE IN       CARD1 (1,10) ;                                            SRAN  14

FORMAT IN     CARD2 (X10, 2E20.11),                                     SRAN  15
              CARD3 (X10, 4I5) ;                                        SRAN  16

LIST          SIZES (SWF, SWP, NAME, N) ;                               SRAN  17

FILE OUT      PAPER (1,15) ;                                            SRAN  18

FORMAT OUT    HEAD (X45, "SIMPLE REGRESSION ANALYSIS" //),              SRAN  19
              FORM (X43, "CASE NO.", I11 // X40, "DATA POINTS", I11 //  SRAN  20
              X47, "CONSTANT TERM", //),                                SRAN  21
              AVERAGEF (X25, "AVERAGE X", E16.8, X10,                   SRAN  22
              "AVERAGE Y", E16.8, //),                                  SRAN  23
              VARIANCEF (X25, "VARIANCE X", E16.8, X9,                  SRAN  24
              "VARIANCE Y", E16.8, //),                                 SRAN  25
              RF (X10, "CORRELATION COEFFICIENT", E16.8,                SRAN  26
              X10, "FRACTION OF VARIANCE REMOVED", E16.8, //),          SRAN  27
              HEAD1 (X47, "Y = AX + B" //),                             SRAN  28
              AF(X5, "SLOPE", E16.8, X10, "SLOPE ERROR",                SRAN  29
              E16.8, X10, "SLOPE T-VALUE", E16.8, //),                  SRAN  30
              BF ("INTERCEPT", E16.8, X10, "INTERCEPT ERROR", E16.8,    SRAN  31
              X10, "INTERCEPT T-VALUE", E16.8, //),                     SRAN  32
              ERRORF (X18, "VARIANCE OF FIT", E16.8, X10,               SRAN  33
              "STANDARD ERROR OF ESTIMATE", E16.8, //),                 SRAN  34
              HEAD2 (X47, "X = AY + B" //),                             SRAN  35
              RESIDUALS (// // X23, "INDEPENDENT", X11, "DEPENDENT",    SRAN  36
              X13, "ESTIMATE", X13, "RESIDUAL" //),                     SRAN  37
              RESF (X20, 4(E16.8, X5)),                                 SRAN  38
              FORCE (/ X45, "NO CONSTANT TERM" //),                     SRAN  39
              TITLE1 (X49, "Y = AX" //),                                SRAN  40
              TITLE2 (X49, "X = AY" //) ;                               SRAN  41

LIST          CASE (NAME, N),                                           SRAN  42
              AVERAGEO (AVERAGEX, AVERAGEY),                            SRAN  43
              VARIANCEO (VARIANCEX, VARIANCEY),                         SRAN  44
              RO (R, R | R),                                            SRAN  45
              AO (A, SA, TA),                                           SRAN  46
              BO (B, SB, TB),                                           SRAN  47
              ERRORO (VARIANCE, SE),                                    SRAN  48
              RESO (IJ, DJ, ESTIMATE, RESIDUAL),                        SRAN  49
              RFOO (R, R2) ;                                            SRAN  50

LABEL         START ;                                                   SRAN  51

START :       READ (CARD1, CARD3, SIZES) ;                              SRAN  52
              WRITE (PAPER, HEAD) ;                                     SRAN  53
              WRITE (PAPER, FORM, CASE) ;                               SRAN  54
              SUMX ~ SUMY ~ SUMXY ~ SUMX2 ~ SUMY2 ~                     SRAN  55
              DXY ~ DX2 ~ DY2 ~ 0 ;                                     SRAN  56

         BEGIN                                                          SRAN  57
REAL ARRAY    X[0:N], Y[0:N] ;                                          SRAN  58

COMMENT       THE PROCEDURE PRINTDIFF COMPUTES THE RESIDUALS AND PRINTS SRAN  59
              THEM ALONG WITH THE CORRESPONDING DATA ;                  SRAN  60

PROCEDURE     PRINTDIFF ;                                               SRAN  61
         BEGIN                                                          SRAN  62
              WRITE (PAPER, RESIDUALS) ;                                SRAN  63
              FOR J ~ 1 STEP 1 UNTIL N DO                               SRAN  64
         BEGIN                                                          SRAN  65
              IF S THEN                                                 SRAN  66
         BEGIN                                                          SRAN  67
              IJ ~ X[J] ;         DJ ~ Y[J]                             SRAN  68
         END                                                            SRAN  69
              ELSE                                                      SRAN  70
         BEGIN                                                          SRAN  71
              IJ ~ Y[J] ;         DJ ~ X[J]                             SRAN  72
         END ;                                                          SRAN  73
              ESTIMATE ~ A | IJ + B ;                                   SRAN  74
              RESIDUAL ~ DJ - ESTIMATE ;                                SRAN  75
              WRITE (PAPER, RESF, RESO)                                 SRAN  76
         END ;                                                          SRAN  77
              WRITE (PAPER [PAGE], RESF)                                SRAN  78
         END PRINTDIFF ;                                                SRAN  79

LIST          DATA (X[J], Y[J]) ;                                       SRAN  80

LABEL         FORCELINE ;                                               SRAN  81

              FOR J ~ 1 STEP 1 UNTIL N DO                               SRAN  82
         BEGIN                                                          SRAN  83
              READ (CARD1, CARD2, DATA) ;                               SRAN  84
              IJ ~ X[J] ;         DJ ~ Y[J] ;                           SRAN  85
              SUMX ~ IJ + SUMX ;                                        SRAN  86
              SUMY ~ DJ + SUMY ;                                        SRAN  87
              SUMXY ~ IJ | DJ + SUMXY ;                                 SRAN  88
              SUMX2 ~ IJ * 2 + SUMX2 ;                                  SRAN  89
              SUMY2 ~ DJ * 2 + SUMY2                                    SRAN  90
         END ;                                                          SRAN  91
              AVERAGEX ~ SUMX / N ;                                     SRAN  92
              AVERAGEY ~ SUMY / N ;                                     SRAN  93
              WRITE (PAPER, AVERAGEF, AVERAGEO) ;                       SRAN  94
              FOR J ~ 1 STEP 1 UNTIL N DO                               SRAN  95
         BEGIN                                                          SRAN  96
              DIFFX ~ AVERAGEX - X[J] ;                                 SRAN  97
              DIFFY ~ AVERAGEY - Y[J] ;                                 SRAN  98
              DXY ~ DIFFX | DIFFY + DXY ;                               SRAN  99
              DX2 ~ DIFFX * 2 + DX2 ;                                   SRAN 100
              DY2 ~ DIFFY * 2 + DY2                                     SRAN 101
         END ;                                                          SRAN 102
              VARIANCEX ~ DX2 / (N-1) ;                                 SRAN 103
              VARIANCEY ~ DY2 / (N-1) ;                                 SRAN 104
              WRITE (PAPER, VARIANCEF, VARIANCEO) ;                     SRAN 105

COMMENT       COMPUTE THE CORRELATION COEFFICIENT ;                     SRAN 106
              R ~ DXY / SQRT (DX2 | DY2) ;                              SRAN 107
              WRITE (PAPER, RF, RO) ;                                   SRAN 108

COMMENT       COMPUTE REGRESSION COEFFICIENTS FOR Y = F(X) AND X = F(Y) SRAN 109
              ALONG WITH OTHER RELATED DATA ;                           SRAN 110
              WRITE (PAPER, HEAD1) ;                                    SRAN 111
              A ~ DXY / DX2 ;                                           SRAN 112
              B ~ (SUMY - A | SUMX) / N ;                               SRAN 113
              VARIANCE ~ (DY2 - A | DXY) / (N-2) ;                      SRAN 114
              SE ~ SQRT (VARIANCE) ;                                    SRAN 115
              SA ~ SE / SQRT (DX2) ;                                    SRAN 116
              TA ~ A / SA ;                                             SRAN 117
              WRITE (PAPER, AF, AO) ;                                   SRAN 118
              SB ~ SE | SQRT (SUMX2 / (N | DX2)) ;                      SRAN 119
              TB ~ B/SB ;                                               SRAN 120
              WRITE (PAPER, BF, BO) ;                                   SRAN 121
              WRITE (PAPER, ERRORF, ERRORO) ;                           SRAN 122
              IF SWP THEN                                               SRAN 123
         BEGIN                                                          SRAN 124
              S ~ TRUE ;          PRINTDIFF                             SRAN 125
         END ;                                                          SRAN 126

              WRITE (PAPER, HEAD2) ;                                    SRAN 127
              A ~ DXY / DY2 ;                                           SRAN 128
              B ~ (SUMX - A | SUMY) / N ;                               SRAN 129
              VARIANCE ~ (DX2 - A | DXY) / (N-2) ;                      SRAN 130
              SE ~ SQRT (VARIANCE) ;                                    SRAN 131
              SA ~ SE / SQRT (DY2) ;                                    SRAN 132
              TA ~ A / SA ;                                             SRAN 133
              WRITE (PAPER, AF, AO) ;                                   SRAN 134
              SB ~ SE | SQRT (SUMY2 / (N | DY2)) ;                      SRAN 135
              TB ~ B / SB ;                                             SRAN 136
              WRITE (PAPER, BF, BO) ;                                   SRAN 137
              WRITE (PAPER, ERRORF, ERRORO) ;                           SRAN 138
              IF SWP THEN                                               SRAN 139
         BEGIN                                                          SRAN 140
              S ~ FALSE ;          PRINTDIFF                            SRAN 141
         END ;                                                          SRAN 142
              IF NOT SWF THEN GO TO START ;                             SRAN 143

FORCELINE :   WRITE (PAPER, FORCE) ;                                    SRAN 144
              B ~ R ~ 0 ;                                               SRAN 145
              A ~ SUMXY / SUMX2 ;                                       SRAN 146
              R2 ~ (A | SUMXY - SUMY | SUMY / N) / DY2 ;                SRAN 147
              WRITE (PAPER, TITLE1) ;                                   SRAN 148
              WRITE (PAPER, RF, RFOO) ;                                 SRAN 149
              VARIANCE ~ (SUMY2 - A | SUMXY) / (N-1) ;                  SRAN 150
              SE ~ SQRT (VARIANCE) ;                                    SRAN 151
              SA ~ SE / SQRT (SUMX2) ;                                  SRAN 152
              TA ~ A / SA ;                                             SRAN 153
              WRITE (PAPER, AF, AO) ;                                   SRAN 154
              WRITE (PAPER, ERRORF, ERRORO) ;                           SRAN 155
              IF SWP THEN                                               SRAN 156
         BEGIN                                                          SRAN 157
              S ~ TRUE ;          PRINTDIFF                             SRAN 158
         END ;                                                          SRAN 159

              WRITE (PAPER, TITLE2) ;                                   SRAN 160
              A ~ SUMXY / SUMY2 ;                                       SRAN 161
              R2 ~ (A | SUMXY - SUMX | SUMX / N) / DX2 ;                SRAN 162
              WRITE (PAPER, RF, RFOO) ;                                 SRAN 163
              VARIANCE ~ (SUMX2 - A | SUMXY) / (N-1) ;                  SRAN 164
              SE ~ SQRT (VARIANCE) ;                                    SRAN 165
              SA ~ SE / SQRT (SUMY2) ;                                  SRAN 166
              TA ~ A / SA ;                                             SRAN 167
              WRITE (PAPER, AF, AO) ;                                   SRAN 168
              WRITE (PAPER, ERRORF, ERRORO) ;                           SRAN 169
              IF SWP THEN                                               SRAN 170
         BEGIN                                                          SRAN 171
              S ~ FALSE ;          PRINTDIFF                            SRAN 172
         END ;                                                          SRAN 173
              GO TO START                                               SRAN 174
         END                                                            SRAN 175
         END BIVARIATE ;                                                SRAN 176
         END .                                                          SRAN 177

 LABEL  000000000II     0010000001
              1    1   11   16
             1.1          @      0.109        @  2
             1.8          @      0.1145       @  2
             2.5          @      0.12         @  2
             2.2          @      0.121        @  2
             3.1          @      0.1245       @  2
             2.7          @      0.123        @  2
             2.6          @      0.1235       @  2
             3.5          @      0.129        @  2
             2.4          @      0.1185       @  2
             2.6          @      0.12         @  2
             2.5          @      0.1225       @  2
             2.1          @      0.1175       @  2
             0.9          @      0.109        @  2
             1.9          @      0.116        @  2
             2.8          @      0.1215       @  2
             1.8          @      0.1145       @  2

