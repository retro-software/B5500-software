         BEGIN                                                          MAIN   1
                                                                        MAIN   2
COMMENT       ON LINE PLOTTER  .                                        MAIN   3
                                                                        MAIN   4
              ROBERT R. HAMMERS ,                                       MAIN   5
              PROFESSIONAL SERVISES GROUP ,                             MAIN   6
              BURROUGHS CORPORATION ,                                   MAIN   7
              PASADENA , CALIFORNIA                                     MAIN   8
                                                                        MAIN   9
                                                                        MAIN  10
              FIRST RELEASE 1 - 1 - 64  ,                               MAIN  11
                                                                        MAIN  12
              CARD SEQUENCE FOR MAIN PROGRAM STARTS WITH MAIN0001 ,     MAIN  13
              CARD SEQUENCE FOR PROCEDURES   STARTS WITH PLOT0001      ;MAIN  14
                                                                        MAIN  15
                                                                        MAIN  16
                                                                        MAIN  17
FILE IN       RDR (1,10)  ;                                             MAIN  18
FILE OUT      IMG (10,15)  ;                                            MAIN  19
INTEGER       NHL,NSBH,NVL,NSBV,TNHL,TNVL,NDATA,M,N  ;                  MAIN  20
BOOLEAN       GRID,LABELS  ;                                            MAIN  21
LABEL         START,FINISH  ;                                           MAIN  22
FORMAT IN     F1 (9I5,2L5)  ;                                           MAIN  23
                                                                        MAIN  24
REAL          I  ;                                                      MAIN  25
                                                                        MAIN  26
START:        READ (RDR,F1,NHL,NSBH,NVL,NSBV,TNHL,TNVL,NDATA,M,N,GRID,  MAIN  27
              LABELS) [FINISH]  ;                                       MAIN  28
                                                                        MAIN  29
         BEGIN                                                          MAIN  30
                                                                        MAIN  31
ALPHA ARRAY   IMAGE [0:TNHL,0:19]  ;                                    MAIN  32
                                                                        MAIN  33
REAL ARRAY    ORD[0:TNHL],ABSCIS[0:TNVL],X,Y[0:NDATA]  ;                MAIN  34
                                                                        MAIN  35
PROCEDURE     PLOT1(IMAGE,NHL,NSBH,NVL,NSBV,ABSCIS,TNHL,TNVL,GRID,ORD) ;PLOT   1
                                                                        PLOT   2
VALUE         NHL,NSBH,NVL,NSBV,TNHL,TNVL  ;                            PLOT   3
REAL ARRAY    ABSCIS,ORD[0]  ;                                          PLOT   4
ALPHA ARRAY   IMAGE[0,0]  ;                                             PLOT   5
INTEGER       NHL,NSBH,NVL,NSBV,TNHL,TNVL  ;                            PLOT   6
BOOLEAN       GRID  ;                                                   PLOT   7
                                                                        PLOT   8
         BEGIN                                                          PLOT   9
                                                                        PLOT  10
ALPHA ARRAY   ISIX[0:6]  ;                                              PLOT  11
                                                                        PLOT  12
INTEGER       I,J,JU1  ;                                                PLOT  13
REAL          EACH,XMIN,XMAX,YMIN,YMAX  ;                               PLOT  14
                                                                        PLOT  15
              XMIN ~ ABSCIS[0]  ;  XMAX ~ ABSCIS[TNVL]  ;               PLOT  16
              YMIN ~ ORD[TNHL]  ;  YMAX ~ ORD[0]  ;                     PLOT  17
              JU1 ~ TNVL DIV 6  ;  IF TNHL MOD 6 ! 0 THEN JU1 ~ JU1+1  ;PLOT  18
                                                                        PLOT  19
COMMENT       THE BOOLEAN "GRID" = TRUE CAUSES A HORIZONTAL AND VERTICALPLOT  20
              GRID TO BE PLOTTED IN THE IMAGE MATRIX . "GRID" = FALSE   PLOT  21
              SUPPRESSES THESE GRID LINES  .  ;                         PLOT  22
                                                                        PLOT  23
              IF GRID THEN                                              PLOT  24
         BEGIN                                                          PLOT  25
           FILL ISIX[*] WITH "      ",":     "," :    ","  :   ",       PLOT  26
              "   :  ","    : ","     :"  ;                             PLOT  27
                                                                        PLOT  28
              FOR I ~ 1 STEP 1 UNTIL TNHL DO                            PLOT  29
         BEGIN                                                          PLOT  30
              FOR J ~ 1  STEP NSBV UNTIL TNVL DO                        PLOT  31
              IMAGE[I,(J DIV 6)+1] ~ ISIX[(J MOD 6)]  ;                 PLOT  32
              IF I MOD NSBH = 0 THEN                                    PLOT  33
         BEGIN                                                          PLOT  34
              J ~ 1  ;  DO                                              PLOT  35
         BEGIN                                                          PLOT  36
              IMAGE[I,J] ~ "------"                                     PLOT  37
         END                                                            PLOT  38
              UNTIL (J ~ J+1) > JU1                                     PLOT  39
         END                                                            PLOT  40
         END                                                            PLOT  41
         END  ;                                                         PLOT  42
                                                                        PLOT  43
COMMENT       THE "ABSCIS" AND "ORD" VECTORS ARE FILLED WITH UNIFORMLY  PLOT  44
              INCREASING INCREMENTS OF X AND Y FORM ONE LIMIT TO THE    PLOT  45
              OTHER  E.G. ABSCIS[0] = XMIN  ,  ORD[0] = YMAX  .  ;      PLOT  46
                                                                        PLOT  47
              EACH ~ ABS((XMAX-XMIN))/TNVL  ;                           PLOT  48
              JU1 ~ TNVL-1  ;                                           PLOT  49
              FOR J ~ 1 STEP 1 UNTIL JU1 DO                             PLOT  50
              ABSCIS[J] ~ J|EACH + XMIN  ;                              PLOT  51
              EACH ~ ABS((YMAX-YMIN))/TNHL  ;                           PLOT  52
              JU1 ~ TNHL-1  ;                                           PLOT  53
              FOR J ~ 1 STEP 1 UNTIL JU1 DO                             PLOT  54
              ORD[J] ~ -J|EACH + YMAX                                   PLOT  55
         END  ;                                                         PLOT  56
                                                                        PLOT  57
PROCEDURE     PLOT3 (ABSCIS,ORD,X,Y,NDATA,IMAGE,O,FILE1,TNHL,TNVL)  ;   PLOT  58
                                                                        PLOT  59
VALUE         NDATA,O,TNHL,TNVL  ;                                      PLOT  60
REAL ARRAY    ABSCIS,ORD,X,Y[0]  ;                                      PLOT  61
ALPHA ARRAY   IMAGE[0,0]  ;                                             PLOT  62
ALPHA         O  ;                                                      PLOT  63
INTEGER       NDATA,TNHL,TNVL  ;                                        PLOT  64
FILE OUT      FILE1  ;                                                  PLOT  65
                                                                        PLOT  66
         BEGIN                                                          PLOT  67
                                                                        PLOT  68
INTEGER       I,I1,J1,K,M  ;                                            PLOT  69
FORMAT OUT    FO ("THE POINT (X,Y) = (",2F10.5,") IS OUT OF RANGE"/)  ; PLOT  70
REAL          TEMX,TEMY  ;                                              PLOT  71
LABEL         IEND,F1,F2,F3,F4,F5,IEND1  ;                              PLOT  72
SWITCH        SW ~ F1,F2,F3,F4,F5  ;                                    PLOT  73
                                                                        PLOT  74
              I ~ 1  ;  DO                                              PLOT  75
         BEGIN                                                          PLOT  76
                                                                        PLOT  77
COMMENT       TEMX AND TEMY ARE THE COORDINATES OF THE POINT CURRENTLY  PLOT  78
              BEING TESTED FOR PLOTTING INTO THE IMAGE MATRIX  .  ;     PLOT  79
                                                                        PLOT  80
              TEMX ~ X[I]  ;  TEMY ~ Y[I]  ;  IF (TEMX < ABSCIS[0]) OR  PLOT  81
              (ABSCIS[TNVL] < TEMX) OR (TEMY < ORD[TNHL]) OR (ORD[0] <  PLOT  82
              TEMY) THEN                                                PLOT  83
         BEGIN                                                          PLOT  84
              WRITE (FILE1,FO,TEMX,TEMY)  ;  GO TO IEND1                PLOT  85
         END  ;                                                         PLOT  86
              I1 ~ -1 ;  DO I1 ~ I1+1 UNTIL (ABSCIS[I1] { TEMX AND TEMX PLOT  87
              { ABSCIS[I1+1])  ;  IF ABS(TEMX - ABSCIS[I1]) >           PLOT  88
              ABS(ABSCIS[I1+1] - TEMX) THEN I1 ~ I1+1  ;                PLOT  89
              J1 ~ -1 ;  DO J1 ~ J1+1 UNTIL (ORD[J1] } TEMY AND TEMY }  PLOT  90
              ORD[J1+1])  ;  IF ABS(ORD[J1] - TEMY) >ABS(TEMY           PLOT  91
              - ORD[J1+1]) THEN J1 ~ J1+1  ;                            PLOT  92
              K ~ (I1 DIV 6) + 1  ;  M ~ I1 MOD 6  ;                    PLOT  93
              TEMX ~ IMAGE[J1+1,K]  ;  GO TO SW[M]  ;                   PLOT  94
              TEMX ~ TEMX&O[12:42:6]  ;  GO TO IEND  ;                  PLOT  95
F1:           TEMX ~ TEMX&O[18:42:6]  ;  GO TO IEND  ;                  PLOT  96
F2:           TEMX ~ TEMX&O[24:42:6]  ;  GO TO IEND  ;                  PLOT  97
F3:           TEMX ~ TEMX&O[30:42:6]  ;  GO TO IEND  ;                  PLOT  98
F4:           TEMX ~ TEMX&O[36:42:6]  ;  GO TO IEND  ;                  PLOT  99
F5:           TEMX ~ TEMX&O[42:42:6]  ;                                 PLOT 100
IEND:         IMAGE[J1+1,K] ~ TEMX  ;                                   PLOT 101
IEND1:   END                                                            PLOT 102
              UNTIL (I ~ I+1) > NDATA                                   PLOT 103
         END  ;                                                         PLOT 104
                                                                        PLOT 105
PROCEDURE     PLOT4 (IMAGE,ABSCIS,ORD,FILE2,TNVL,NSBV,TNHL,NSBH,N,M,    PLOT 106
              LABELS)  ;                                                PLOT 107
                                                                        PLOT 108
VALUE         TNVL,NSBV,TNHL,NSBH,M,N,LABELS  ;                         PLOT 109
INTEGER       TNHL,NSBV,TNVL,NSBH,M,N  ;                                PLOT 110
BOOLEAN       LABELS  ;                                                 PLOT 111
REAL ARRAY    ABSCIS,ORD[0]   ;                                         PLOT 112
ALPHA ARRAY   IMAGE[0,0]  ;                                             PLOT 113
FILE OUT      FILE2   ;                                                 PLOT 114
                                                                        PLOT 115
         BEGIN                                                          PLOT 116
                                                                        PLOT 117
INTEGER       I,J,K,SIGNM,I1,II,DOT  ;                                  PLOT 118
REAL          OTEMP  ;                                                  PLOT 119
ALPHA ARRAY   A[0:1]  ;                                                 PLOT 120
ALPHA         OCT0,OCT1,OCT2,I0  ;                                      PLOT 121
LABEL         AXISERR,IEND,AL1,AL2,AL3,AL4,AL5,PRINT  ;                 PLOT 122
SWITCH        SW ~ AL1,AL2,AL3,AL4,AL5  ;                               PLOT 123
FORMAT        FERR ("THE VALUES OF THE ABSCISSA OR ORDINATE ARE OUT OF",PLOT 124
              " RANGE EVEN WITH THE PROVIDED SCALE FACTORS (M,N) =",2I6 PLOT 125
                                                                    /) ,PLOT 126
              FIMAGE (20A6)  ,                                          PLOT 127
              FSCALE ("THE ABSCISSA ARE ",I3," POWER OF TEN OF THEIR ", PLOT 128
              "ACTUAL VALUE"/"THE ORDINATE ARE ",I3," POWER OF TEN OF ",PLOT 129
              "THEIR ACTUAL VALUE"//)  ;                                PLOT 130
                                                                        PLOT 131
PROCEDURE     ROUND (OTEMP,A,OCT0,OCT1,OCT2)  ;                         PLOT 132
                                                                        PLOT 133
VALUE         OTEMP  ;                                                  PLOT 134
REAL          OTEMP  ;                                                  PLOT 135
ALPHA ARRAY   A[0]   ;                                                  PLOT 136
ALPHA         OCT0,OCT1,OCT2  ;                                         PLOT 137
                                                                        PLOT 138
         BEGIN                                                          PLOT 139
                                                                        PLOT 140
STREAM PROCEDURE CONVERT(X,A)  ;                                        PLOT 141
                                                                        PLOT 142
         BEGIN                                                          PLOT 143
                                                                        PLOT 144
LOCAL         ST  ;                                                     PLOT 145
              SI ~ X  ;  DI ~ LOC ST  ;  DS ~ 2 DEC  ;                  PLOT 146
              SI ~ LOC ST  ;  DI ~ A  ;  2(DI ~ DI + 7  ;  DS ~ CHR)    PLOT 147
         END  ;                                                         PLOT 148
                                                                        PLOT 149
INTEGER       INT,MANT  ;                                               PLOT 150
REAL          TEMP  ;                                                   PLOT 151
LABEL         AO1,AO2,AO3,AO4,AO5,AO6,CONDEC  ;                         PLOT 152
SWITCH        SWO ~ AO1,AO2,AO3,AO4,AO5,AO6  ;                          PLOT 153
                                                                        PLOT 154
              GO TO SWO[OTEMP.[3:6] - 10]  ;                            PLOT 155
              MANT ~ 0 & OTEMP[9:18:30]  ;                              PLOT 156
              INT  ~ 0 & OTEMP[39:9:9]  ;  GO TO CONDEC  ;              PLOT 157
AO1:          MANT ~ 0 & OTEMP[9:15:33]  ;                              PLOT 158
              INT  ~ 0 & OTEMP[42:9:6 ]  ;  GO TO CONDEC  ;             PLOT 159
AO2:          MANT ~ 0 & OTEMP[9:12:36]  ;                              PLOT 160
              INT  ~ 0 & OTEMP[45:9:3 ]  ;  GO TO CONDEC  ;             PLOT 161
AO3:          MANT ~ 0 & OTEMP[9:9:39 ]  ;                              PLOT 162
              INT  ~ 0  ;  GO TO CONDEC  ;                              PLOT 163
AO4:          MANT ~ 0 & OTEMP[12:9:36]  ;                              PLOT 164
              INT  ~ 0  ;  GO TO CONDEC  ;                              PLOT 165
AO5:          MANT ~ 0 & OTEMP[15:9:33]  ;                              PLOT 166
              INT  ~ 0  ;  GO TO CONDEC  ;                              PLOT 167
AO6:          MANT ~ 0 & OTEMP[18:9:30]  ;  INT ~ 0  ;                  PLOT 168
CONDEC:       TEMP ~ (MANT.[9:3] / 8 + MANT.[12:3] / 64 + MANT.[15:3] / PLOT 169
              512 + MANT.[18:3] / 4096 + MANT.[21:3] / 32768 + MANT.[24:PLOT 170
              3] / 262144 + MANT.[27:3] / 2097152 + MANT.[30:3] /       PLOT 171
              16777216 + MANT.[33:3] / 134217728) | 10  ;               PLOT 172
              OCT0 ~ ENTIER (TEMP)  ;                                   PLOT 173
              TEMP ~ (TEMP - OCT0) | 10  ;                              PLOT 174
              OCT1 ~ ENTIER (TEMP)  ;                                   PLOT 175
              TEMP ~ (TEMP - OCT1) | 10  ;                              PLOT 176
              OCT2 ~ ENTIER (TEMP)  ;                                   PLOT 177
              IF ENTIER ((TEMP - OCT2) | 10) } 5 THEN                   PLOT 178
         BEGIN                                                          PLOT 179
              IF OCT2 ~ OCT2 + 1 } 10 THEN                              PLOT 180
         BEGIN                                                          PLOT 181
              OCT2 ~ "     0"  ;  IF OCT1 ~ OCT1 + 1 } 10 THEN          PLOT 182
         BEGIN                                                          PLOT 183
              OCT1 ~ "     0"  ;  IF OCT0 ~ OCT0 + 1 } 10 THEN          PLOT 184
         BEGIN                                                          PLOT 185
              OCT0 ~ "     0"  ;  INT ~ INT + 1                         PLOT 186
         END                                                            PLOT 187
         END                                                            PLOT 188
         END                                                            PLOT 189
         END  ;                                                         PLOT 190
              CONVERT (INT,A)  ;  IF A[0] = 0 THEN A[0] ~ "      "      PLOT 191
         END ROUND  ;                                                   PLOT 192
                                                                        PLOT 193
              IF LABELS THEN                                            PLOT 194
         BEGIN                                                          PLOT 195
              DOT ~ "."  ;  I0 ~ "      "  ;                            PLOT 196
              FOR I ~ 0 STEP NSBH UNTIL TNHL DO                         PLOT 197
         BEGIN                                                          PLOT 198
              OTEMP ~ ORD[I]|(10*(-N))  ;                               PLOT 199
              IF ABS(OTEMP) } 99.9995 THEN GO TO AXISERR ;              PLOT 200
              SIGNM ~ IF OTEMP.[1:1] = 1 THEN "-" ELSE " "  ;           PLOT 201
                                                                        PLOT 202
              ROUND (OTEMP,A,OCT0,OCT1,OCT2)  ;                         PLOT 203
                                                                        PLOT 204
              I1 ~ IF I = 0 THEN 1 ELSE I  ;                            PLOT 205
              IMAGE[I1,0] ~ I0 & SIGNM[12:42:6] & A[0][18:42:6] & A[1]  PLOT 206
              [24:42:6] & DOT[30:42:6] & OCT0[36:42:6] & OCT1[42:42:6] ;PLOT 207
              IMAGE[I1,1] ~ IMAGE[I1,1] & OCT2[12:42:6]                 PLOT 208
         END  ;                                                         PLOT 209
              FOR I ~ 0 STEP NSBV UNTIL TNVL DO                         PLOT 210
         BEGIN                                                          PLOT 211
              OTEMP ~ ABSCIS[I]|(10*(-M))  ;                            PLOT 212
              IF ABS(OTEMP) } 99.9995 THEN GO TO AXISERR ;              PLOT 213
              SIGNM ~ IF OTEMP.[1:1] = 1 THEN "-" ELSE " "  ;           PLOT 214
                                                                        PLOT 215
              ROUND (OTEMP,A,OCT0,OCT1,OCT2)  ;                         PLOT 216
              II ~ I MOD 6  ;  J ~ I DIV 6  ;                           PLOT 217
              GO TO SW[II]  ;                                           PLOT 218
              IMAGE[0,J]   ~ IMAGE[0,J] & SIGNM[24:42:6] & A[0][30:42:6]PLOT 219
                             & A[1][36:42:6] & DOT[42:42:6]  ;          PLOT 220
              IMAGE[0,J+1] ~ I0 & OCT0[12:42:6] & OCT1[18:42:6] & OCT2  PLOT 221
                             [24:42:6]  ;  GO TO IEND  ;                PLOT 222
AL1:          IMAGE[0,J]   ~ IMAGE[0,J] & SIGNM[30:42:6] & A[0][36:42:6]PLOT 223
                             & A[1][42:42:6]  ;                         PLOT 224
              IMAGE[0,J+1] ~ I0 & DOT[12:42:6] & OCT0[18:42:6] & OCT1   PLOT 225
                             [24:42:6] & OCT2[30:42:6]  ;  GO TO IEND  ;PLOT 226
AL2:          IMAGE[0,J]   ~ IMAGE[0,J] & SIGNM[36:42:6]                PLOT 227
                             & A[0][42:42:6]  ;                         PLOT 228
              IMAGE[0,J+1] ~ I0 & A[1][12:42:6] & DOT[18:42:6] & OCT0   PLOT 229
                             [24:42:6] & OCT1[30:42:6] & OCT2[36:42:6] ;PLOT 230
                             GO TO IEND  ;                              PLOT 231
AL3:          IMAGE[0,J]   ~ IMAGE[0,J] & SIGNM[42:42:6]  ;             PLOT 232
              IMAGE[0,J+1] ~ I0 & A[0][12:42:6] & A[1][18:42:6] & DOT   PLOT 233
                             [24:42:6] & OCT0[30:42:6] & OCT1[36:42:6] &PLOT 234
                             OCT2[42:42:6]  ;  GO TO IEND  ;            PLOT 235
AL4:          IMAGE[0,J+1] ~ I0 & SIGNM[12:42:6] & A[0][18:42:6] & A[1] PLOT 236
                             [24:42:6] & DOT[30:42:6] & OCT0[36:42:6] & PLOT 237
                             OCT1[42:42:6]  ;                           PLOT 238
              IMAGE[0,J+2] ~ I0 & OCT2[12:42:6]  ;  GO TO IEND  ;       PLOT 239
AL5:          IMAGE[0,J+1] ~ I0 & SIGNM[18:42:6] & A[0][24:42:6] & A[1] PLOT 240
                             [30:42:6] & DOT[36:42:6] & OCT0[42:42:6]  ;PLOT 241
              IMAGE[0,J+2] ~ I0 & OCT1[12:42:6] & OCT2[18:42:6]  ;      PLOT 242
IEND:    END  ;                                                         PLOT 243
              WRITE (FILE2,FSCALE,-M,-N)  ;  GO TO PRINT  ;             PLOT 244
AXISERR:      WRITE (FILE2,FERR,M,N)                                    PLOT 245
         END  ;                                                         PLOT 246
PRINT:        WRITE (FILE2,FIMAGE,FOR I ~ 0 STEP 1 UNTIL TNHL DO        PLOT 247
                     FOR J ~ 0 STEP 1 UNTIL 19 DO IMAGE[I,J])           PLOT 248
         END  ;                                                         PLOT 249
                                                                        MAIN  36
              FOR I ~ 0 STEP 1 UNTIL TNHL DO                            MAIN  37
              FILL IMAGE[I,*] WITH "      ","      ","      ","      ", MAIN  38
              "      ","      ","      ","      ","      ","      ",    MAIN  39
              "      ","      ","      ","      ","      ","      ",    MAIN  40
              "      ","      ","      ","      "  ;                    MAIN  41
                                                                        MAIN  42
              FOR I ~ .1 STEP .1 UNTIL 10 DO                            MAIN  43
         BEGIN                                                          MAIN  44
              Y[10|I] ~ I  ;  X[10|I] ~ COS(I)                          MAIN  45
         END  ;                                                         MAIN  46
              ABSCIS[0] ~ -1.0  ;  ABSCIS[TNVL] ~ 1.0  ;  ORD[0] ~ 10.0;MAIN  47
              ORD[TNHL] ~ 0  ;                                          MAIN  48
                                                                        MAIN  49
              PLOT1(IMAGE,NHL,NSBH,NVL,NSBV,ABSCIS,TNHL,TNVL,GRID,ORD) ;MAIN  50
                                                                        MAIN  51
              PLOT3 (ABSCIS,ORD,X,Y,NDATA,IMAGE,"O",IMG,TNHL,TNVL)  ;   MAIN  52
                                                                        MAIN  53
              FOR I ~ .1 STEP .1 UNTIL 10 DO                            MAIN  54
         BEGIN                                                          MAIN  55
              Y[10|I] ~ I  ;  X[10|I] ~ SIN(I)                          MAIN  56
         END  ;                                                         MAIN  57
                                                                        MAIN  58
              PLOT3 (ABSCIS,ORD,X,Y,NDATA,IMAGE,"X",IMG,TNHL,TNVL)  ;   MAIN  59
                                                                        MAIN  60
              PLOT4 (IMAGE,ABSCIS,ORD,IMG,TNVL,NSBV,TNHL,NSBH,N,M,      MAIN  61
              LABELS)  ;                                                MAIN  62
                                                                        MAIN  63
              GO TO START                                               MAIN  64
         END  ;                                                         MAIN  65
                                                                        MAIN  66
FINISH:  END  .                                                         MAIN  67
