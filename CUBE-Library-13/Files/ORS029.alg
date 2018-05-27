         BEGIN                                                          MAPS 001
COMMENT       MANPOWER SCHEDULING -- PRECEDENCE FACTOR METHOD,          MAPS 002
                                     ONE SKILL.                         MAPS 003
              ROBIN T OLLIVIER                                          MAPS 004
              PROFESSIONAL SERVICES GROUP                               MAPS 005
              BURROUGHS CORPORATION                                     MAPS 006
              CARD SEQUENCE BEGINS AT MAPS-0001                         MAPS 007
              FIRST RELEASE  3-15-63  ;                                 MAPS 008
                                                                        MAPS 009
BOOLEAN       PCS1  ;                                                   MAPS 010
                                                                        MAPS 011
REAL          M,MINNO,N,PROJSTART,L ;                                   MAPS 012
                                                                        MAPS 013
ALPHA ARRAY   NAMEP[0:13]  ;                                            MAPS 014
                                                                        MAPS 015
FILE IN       DATA1(1,15)  ;                                            MAPS 016
FORMAT IN     FORM1(13A6,X2/3I10,X4,L5)  ;                              MAPS 017
              READ(DATA1,FORM1,FOR L ~ 1 STEP 1 UNTIL 13 DO NAMEP[L],   MAPS 018
                   N,PROJSTART,MINNO,PCS1) ;                            MAPS 019
                                                                        MAPS 020
              M ~ N DIV 2   ;                                           MAPS 021
         BEGIN                                                          MAPS 022
REAL          A,B,C,CLOCK,D,E,G,H,I,J,JM,K,KJ,L,LOW,MAXMEN,MENUSE,      MAPS 023
              NEXTIME,PASS,PH,POOL,PROJEND,SLK,SMIN,WMIN,WR,HIGH  ;     MAPS 024
                                                                        MAPS 025
INTEGER       Z  ;                                                      MAPS 026
                                                                        MAPS 027
BOOLEAN       SW1,SW2,SW3,SW4  ;                                        MAPS 028
                                                                        MAPS 029
REAL ARRAY    FINIS,ET,ES,LFT,EST,STAR[0:N],NIN,NOUT,FLAG1,FLAG2,Q[0:N],MAPS 030
              TI[0: N],JOB[0:N,0:3],DOJOB[0:M],HOLD[0:M DIV 2]  ;       MAPS 031
                                                                        MAPS 032
LABEL         M1,M2,M3,M4,M5,M6,M7,M8,M9,    VECS,SORT,NETAN  ;         MAPS 033
                                                                        MAPS 034
FORMAT IN     FORM2(A6,X4,4(I8,X2))  ;                                  MAPS 035
                                                                        MAPS 036
FILE          OUT1 1 (1,15) ;   COMMENT     TEMPORARY REPLACEMENT ;     MAPSTEMP
                                                                        MAPS 038
FORMAT OUT    FORM3(X17,I4,"-",I5,X4 ,I6,X5,9(A6,X1)/(X42,9(A6,X1) )) , MAPS 039
                                                                        MAPS 040
              FORM4(X20,"PERIOD",X8 ,"MEN        JOBS IN PROGRESS"//I1),MAPS 041
              FORM5(/X9,"PASS",I4,"-",I3//I1)  ,                        MAPS 042
              FORM6(X20,A6,X2,2I8,I6,X2,A6,I4,I8)  ,                    MAPS 043
              FORM7(X23,"JOB      START / FINISH      COMMENT"/I1)  ,   MAPS 044
              FORM8(X19,"MEN USED THIS PASS",I8I1)  ,                   MAPS 045
              ERROR1(X45,"RANGE EXCEEDED.  SEE CONSTRAINT NO.",I4,I1) , MAPS 046
              ERROR2(X45,"JOB",A6," CRITICAL. PREDECESSOR NOT DONE",I1);MAPS 047
                                                                        MAPS 048
LIST          L1(CLOCK,NEXTIME,MENUSE,FOR L ~ 1 STEP 1 UNTIL A DO       MAPS 049
                 JOB[DOJOB[L],1])  ,                                    MAPS 050
              L2(JOB[I,1],STAR[I],FINIS[I],FLAG1[I],Z,FLAG2[I])  ,      MAPS 051
              L3(JOB[I,1],STAR[I],FINIS[I])  ,                          MAPS 052
              L11(JOB[I,1],JOB[I,2],JOB[I,3],NIN[I],NOUT[I])  ;         MAPS 053
              FOR I ~ 1 STEP 1 UNTIL N DO READ(DATA1,FORM2,L11)  ;      MAPS 054
              G ~ M  ;  KJ ~ JM ~ 9999999999  ;  PASS ~ 1 ;             MAPS 055
              LOW ~ POOL ~ PH ~ 0  ;  SW4 ~ FALSE  ;                    MAPS 056
                                                                        MAPS 057
COMMENT       THIS ROUTINE COMPUTES EST, LFT, AND PROJECT END TIME.  ;  MAPS 058
                                                                        MAPS 059
NETAN:   BEGIN                                                          MAPS 060
REAL          IK,JK,MX,MN  ;                                            MAPS 061
                                                                        MAPS 062
REAL ARRAY    TE[0:N]  ;                                                MAPS 063
                                                                        MAPS 064
FORMAT OUT    FORMN(X20,"PROJECT NAME   ",13A6//X20,"NETWORK ANALYSIS"//MAPS 065
                   X20, "CRITICAL TIME",I6,X4,"AVERAGE NUMBER OF MEN ", MAPS 066
                   "REQUIRED",F8.2//X7,2(X12,"NAME",X5,"MEN   DURATION",MAPS 067
                   "  EST     LFT")/(X7,2(X10,A6,4I8)))  ;              MAPS 068
                                                                        MAPS 069
              FOR I ~ 1 STEP 1 UNTIL N DO                               MAPS 070
         BEGIN                                                          MAPS 071
              ET[I] ~ JOB[I,3]  ; TE[I] ~ TI[I] ~ 0  ;                  MAPS 072
              NIN[I] ~ NIN[I] - MINNO + 1;  NOUT[I] ~ NOUT[I]-MINNO+1  ;MAPS 073
         END ;                                                          MAPS 074
              FOR I ~ 1 STEP 1 UNTIL N DO                               MAPS 075
         BEGIN                                                          MAPS 076
              IK ~ NIN[I]  ;  JK ~ NOUT[I] ;  MX ~ TI[IK] + ET[I]  ;    MAPS 077
              IF (TI[JK] = 0) OR (TI[JK] < MX) THEN TI[JK] ~ MX         MAPS 078
         END ;                                                          MAPS 079
              PROJEND ~ TE[NOUT[N]] ~ TI[NOUT[N]]  ;                    MAPS 080
              FOR I ~ N STEP -1 UNTIL 1 DO                              MAPS 081
         BEGIN                                                          MAPS 082
              IK ~ NIN[I]  ;  JK ~ NOUT[I]  ;  MN ~ TE[JK] - ET[I]  ;   MAPS 083
              IF (TE[IK] = 0) OR (TE[IK] > MN) THEN TE[IK] ~ MN         MAPS 084
         END ;                                                          MAPS 085
              FOR I ~ 1 STEP 1 UNTIL N DO                               MAPS 086
         BEGIN                                                          MAPS 087
              LOW ~ LOW + JOB[I,2] | JOB[I,3]  ;                        MAPS 088
              ES[I] ~ TI[NIN[I]] + PROJSTART  ;  LFT[I] ~ TE[NOUT[I]]   MAPS 089
         END ;                                                          MAPS 090
              LOW ~ ENTIER(LOW / (PROJEND - PROJSTART)) - 1  ;          MAPS 091
              WRITE(OUT1, FORMN, FOR L ~ 1 STEP 1 UNTIL 13 DO           MAPS 092
                   NAMEP[L],PROJEND,LOW,FOR L ~ 1 STEP 1 UNTIL N DO     MAPS 093
                   [JOB[L,1],JOB[L,2],JOB[L,3],ES[L],LFT[L]])           MAPS 094
         END ;                                                          MAPS 095
                                                                        MAPS 096
M1:           NEXTIME ~ PROJSTART  ;  MAXMEN ~ 0  ;  SW2 ~ TRUE  ;      MAPS 097
              FOR I ~ 1 STEP 1 UNTIL N DO                               MAPS 098
         BEGIN                                                          MAPS 099
              FINIS[I] ~ FLAG1[I] ~ FLAG2[I] ~ 0 ;  JOB[I,1].[9:3] ~ 0 ;MAPS 100
              STAR[I] ~ -1 ;  EST[I] ~ ES [I]  ;  ET[I] ~ JOB[I,3]      MAPS 101
         END ;                                                          MAPS 102
              IF SW4 THEN                                               MAPS 103
         BEGIN                                                          MAPS 104
              PH ~ PH + 1  ;  WRITE(OUT1[PAGE],FORM5,PASS-1,PH)  ;      MAPS 105
              WRITE(OUT1,FORM4)                                         MAPS 106
         END ;                                                          MAPS 107
M2:           CLOCK ~ NEXTIME  ;  SW1 ~ FALSE  ;                        MAPS 108
              FOR I ~ 1 STEP 1 UNTIL N DO                               MAPS 109
              IF ET[I] > 0 THEN                                         MAPS 110
         BEGIN                                                          MAPS 111
              IF EST[I] < CLOCK THEN EST[I] ~ CLOCK  ;                  MAPS 112
              Q[I] ~ LFT[I] + EST[I]                                    MAPS 113
         END  ;                                                         MAPS 114
M3:           MENUSE ~ A ~ 0 ; SW3 ~ FALSE ; WMIN ~ 9999 ; E ~ H ~ G+1 ;MAPS 115
                                                                        MAPS 116
COMMENT       SORT BY LEAST Q.  WITHIN Q BY EST.  ;                     MAPS 117
                                                                        MAPS 118
              IF SW2 THEN                                               MAPS 119
SORT:    BEGIN                                                          MAPS 120
REAL          M,TEM,V,VM  ;                                             MAPS 121
                                                                        MAPS 122
LABEL         S1,S2,S3,S4,S5  ;                                         MAPS 123
                                                                        MAPS 124
              M ~ N  ;                                                  MAPS 125
S1:           M ~ M DIV 2  ;                                            MAPS 126
              IF M ! 0  THEN                                            MAPS 127
         BEGIN                                                          MAPS 128
              J ~ N - M  ;  K ~ 1  ;                                    MAPS 129
S2:           V ~ K  ;                                                  MAPS 130
S3:           VM ~ V + M  ;                                             MAPS 131
              IF Q[V] > Q[VM] THEN GO TO S4  ;                          MAPS 132
              IF Q[V] < Q[VM] THEN GO TO S5  ;                          MAPS 133
              IF EST[V] > EST[VM] THEN GO TO S4  ;                      MAPS 134
              GO TO S5  ;                                               MAPS 135
S4:           FOR I ~ 1 STEP 1 UNTIL 3 DO                               MAPS 136
         BEGIN                                                          MAPS 137
              TEM ~ JOB[V,I]; JOB[V,I] ~ JOB[VM,I]; JOB[VM,I] ~ TEM     MAPS 138
         END ;                                                          MAPS 139
              TEM ~ ES[V] ; ES[V] ~ ES[VM] ; ES[VM] ~ TEM  ;            MAPS 140
              TEM ~ NIN[V] ;  NIN[V] ~ NIN[VM] ; NIN[VM] ~ TEM  ;       MAPS 141
              TEM ~ ET[V] ; ET[V] ~ ET[VM] ; ET[VM] ~ TEM  ;            MAPS 142
              TEM ~ Q[V] ; Q[V] ~ Q[VM] ; Q[VM] ~ TEM  ;                MAPS 143
              TEM ~ EST[V] ; EST[V] ~ EST[VM] ; EST[VM] ~ TEM  ;        MAPS 144
              TEM ~ LFT[V] ; LFT[V] ~ LFT[VM] ; LFT[VM] ~ TEM  ;        MAPS 145
              TEM ~ NOUT[V] ;  NOUT[V] ~ NOUT[VM] ;  NOUT[VM] ~ TEM  ;  MAPS 146
              TEM ~ STAR[V] ; STAR[V] ~ STAR[VM] ; STAR[VM] ~ TEM  ;    MAPS 147
              TEM ~ FLAG1[V] ; FLAG1[V] ~ FLAG1[VM] ; FLAG1[VM] ~ TEM  ;MAPS 148
              TEM ~ FLAG2[V] ; FLAG2[V] ~ FLAG2[VM] ; FLAG2[VM] ~ TEM  ;MAPS 149
              TEM ~ FINIS[V] ; FINIS[V] ~ FINIS[VM] ; FINIS[VM] ~ TEM  ;MAPS 150
              V ~ V - M  ;                                              MAPS 151
              IF V } 1 THEN GO TO S3  ;                                 MAPS 152
S5:           K ~ K + 1  ;                                              MAPS 153
              IF K > J THEN GO TO S1  ;                                 MAPS 154
              GO TO S2  ;                                               MAPS 155
         END END  ;                                                     MAPS 156
              SW2 ~ FALSE  ;                                            MAPS 157
                                                                        MAPS 158
COMMENT       FIND STARTABLE JOBS. CHECK PREDECESSORS FOR COMPLETION.  ;MAPS 159
                                                                        MAPS 160
              FOR I ~ 1 STEP 1 UNTIL N DO                               MAPS 161
              IF (EST[I] = CLOCK) AND (ET[I] > 0) THEN                  MAPS 162
         BEGIN                                                          MAPS 163
              SLK ~ LFT[I] - ET[I] - CLOCK  ;                           MAPS 164
              FOR J ~ 1 STEP 1 UNTIL N DO                               MAPS 165
              IF (NOUT[J] = NIN[I]) AND (ET[J] ! 0) THEN                MAPS 166
         BEGIN                                                          MAPS 167
              FLAG2[I] ~ 9999  ;  GO TO M5                              MAPS 168
         END ;                                                          MAPS 169
              IF SW3 THEN GO TO M5  ;                                   MAPS 170
              IF SLK { 0 THEN SW3 ~ TRUE  ;                             MAPS 171
M4:           B ~ MENUSE + JOB[I,2]  ;                                  MAPS 172
              IF (B > POOL) AND (PASS > 1) THEN GO TO M5  ;             MAPS 173
              MENUSE ~ B  ;  A ~ A + 1  ;  DOJOB[A] ~ I  ;              MAPS 174
              IF ET[I] < WMIN THEN WMIN ~ ET[I]  ;                      MAPS 175
              GO TO M6  ;                                               MAPS 176
M5:           E ~ E - 1  ;  DOJOB[E] ~ I  ;                             MAPS 177
M6:           END  ;                                                    MAPS 178
                                                                        MAPS 179
COMMENT       DO MULTIPLE CRITICAL JOBS EXIST  ;                        MAPS 180
                                                                        MAPS 181
M7:           SMIN ~ WMIN  ;                                            MAPS 182
              FOR I ~ E STEP 1 UNTIL G DO                               MAPS 183
         BEGIN                                                          MAPS 184
              J ~ DOJOB[I]  ; SLK ~ LFT[J] - ET[J] - CLOCK  ;           MAPS 185
              IF SMIN > SLK THEN                                        MAPS 186
         BEGIN                                                          MAPS 187
              H ~ I  ;  I ~ G  ;  IF SLK = 0 THEN GO TO M8              MAPS 188
         END END ;                                                      MAPS 189
              FOR I ~ H STEP 1 UNTIL G DO                               MAPS 190
              IF ET[DOJOB[I]] < SMIN THEN BEGIN                         MAPS 191
              WMIN ~ ET[DOJOB[I]]  ;  H ~ I  END ;                      MAPS 192
M8:           IF SMIN ! WMIN THEN GO TO M7  ;                           MAPS 193
              FOR I ~ G STEP -1 UNTIL H DO                              MAPS 194
         BEGIN                                                          MAPS 195
              J ~ DOJOB[I]  ;  B ~ MENUSE + JOB[J,2]  ;  D ~ 0  ;       MAPS 196
              IF (B { POOL) OR (PASS = 1) THEN                          MAPS 197
              IF FLAG2[J] ! 9999 THEN                                   MAPS 198
         BEGIN                                                          MAPS 199
              DOJOB[I] ~ 0  ;  MENUSE ~ B  ;  A ~ A + 1 ;  DOJOB[A] ~ J;MAPS 200
              IF ET[J] < WMIN THEN WMIN ~ ET[J]                         MAPS 201
         END                                                            MAPS 202
              ELSE                                                      MAPS 203
         BEGIN                                                          MAPS 204
              IF LFT[J] - ET[J] - CLOCK < WMIN THEN                     MAPS 205
              IF NOT SW4 THEN GO TO M9 ELSE WRITE(OUT1,ERROR2,JOB[J,1]) MAPS 206
         END                                                            MAPS 207
              ELSE                                                      MAPS 208
         BEGIN                                                          MAPS 209
              IF NOT SW1 THEN                                           MAPS 210
         BEGIN                                                          MAPS 211
              SW1 ~ SW2 ~ TRUE  ;  GO TO M3                             MAPS 212
         END ;                                                          MAPS 213
              IF LFT[J] - ET[J] - CLOCK < WMIN THEN                     MAPS 214
         BEGIN                                                          MAPS 215
              K ~ SLK ~ 0  ;  C ~ B - POOL  ;                           MAPS 216
              WHILE D < C AND K < A DO                                  MAPS 217
         BEGIN                                                          MAPS 218
              L ~ DOJOB[A - K]  ;                                       MAPS 219
              IF LFT[L] - ET[L] - CLOCK } WMIN THEN D ~ D + JOB[L,2]    MAPS 220
              ELSE BEGIN HOLD[SLK ~ SLK+1] ~ L  ;  DOJOB[A-K] ~ 0 END ; MAPS 221
              K ~ K + 1                                                 MAPS 222
         END ;                                                          MAPS 223
              IF D< C THEN IF NOT SW4 THEN GO TO M9                     MAPS 224
              ELSE BEGIN FOR L~1 STEP 1 UNTIL A DO IF DOJOB[L] = 0 THEN MAPS 225
              BEGIN DOJOB[L] ~ HOLD[SLK]; SLK~SLK-1 END END             MAPS 226
              ELSE                                                      MAPS 227
         BEGIN                                                          MAPS 228
              WR ~ K  ;                                                 MAPS 229
              FOR L ~ SLK STEP -1 UNTIL 1 DO                            MAPS 230
         BEGIN                                                          MAPS 231
              WR ~ WR - 1  ;  SMIN ~ A - WR  ;                          MAPS 232
              IF DOJOB[SMIN] ! 0 THEN                                   MAPS 233
         BEGIN                                                          MAPS 234
              E ~ E - 1  ;  DOJOB[E] ~ DOJOB[SMIN]                      MAPS 235
         END ;                                                          MAPS 236
              DOJOB[SMIN] ~ HOLD[L]                                     MAPS 237
         END ;                                                          MAPS 238
              WR ~ A + SLK - K + 1  ;                                   MAPS 239
              FOR L ~ WR STEP 1 UNTIL  A DO                             MAPS 240
              IF DOJOB[L] ! 0 THEN                                      MAPS 241
         BEGIN                                                          MAPS 242
              E ~ E - 1  ;  DOJOB[E] ~ DOJOB[L]                         MAPS 243
         END ;                                                          MAPS 244
              A ~ WR ; DOJOB[I] ~ 0 ; MENUSE ~ B - D  ;  DOJOB[WR] ~ J  MAPS 245
         END END END END ;                                              MAPS 246
                                                                        MAPS 247
COMMENT       FIND SET OF VECTOR COMPONENTS WITH LEAST MEAN INDICE      MAPS 248
              WHOSE SUM IS A GIVEN CONSTANT.  ;                         MAPS 249
                                                                        MAPS 250
VECS:    BEGIN                                                          MAPS 251
REAL          AI,AK,AVC,AVD,CT,F,M,NK,NUM,NUMB,PM,TAL,X  ;              MAPS 252
                                                                        MAPS 253
REAL ARRAY    AB,C,ODE,DJ[0:N DIV 2]  ;                                 MAPS 254
                                                                        MAPS 255
LABEL         V1,V2,V3,V4,V5  ;                                         MAPS 256
                                                                        MAPS 257
              X ~ M ~ NUMB ~ 0  ;  B ~ F ~ 9999  ;                      MAPS 258
              PM ~ POOL - MENUSE  ;                                     MAPS 259
              IF PASS = 1 THEN PM ~ MAXMEN - MENUSE  ;                  MAPS 260
              FOR I ~ G STEP -1 UNTIL E DO                              MAPS 261
         BEGIN                                                          MAPS 262
              J ~ DOJOB[I]  ;  AK ~ JOB[J,2]  ;                         MAPS 263
              IF (AK { PM) AND (J ! 0) AND (FLAG2[J] ! 9999) THEN       MAPS 264
         BEGIN                                                          MAPS 265
              M ~ M + 1  ;  AB[M] ~ AK  ;  ODE[M] ~ I                   MAPS 266
         END  ;                                                         MAPS 267
              IF FLAG2[J] = 9999 THEN FLAG2[J] ~ 0                      MAPS 268
         END ;                                                          MAPS 269
              IF PCS1 THEN                                              MAPS 270
         BEGIN                                                          MAPS 271
              FOR I ~ 1 STEP 1 UNTIL M DO DJ[I] ~ 0  ;                  MAPS 272
V1:           NUM ~ 0  ;                                                MAPS 273
              FOR I ~ 1 STEP 1 UNTIL M DO                               MAPS 274
         BEGIN                                                          MAPS 275
              J ~ AB[I] ;                                               MAPS 276
              IF (J > NUM) AND (J { PM) THEN                            MAPS 277
         BEGIN                                                          MAPS 278
              NUM ~ J  ;  X ~ I                                         MAPS 279
         END END  ;                                                     MAPS 280
              PM ~ PM - NUM  ;  DJ[X] ~ 1  ;  AB[X] ~ 0  ;              MAPS 281
              IF NUM ! 0 THEN GO TO V1  ;                               MAPS 282
              I ~ M  ;  GO TO V5                                        MAPS 283
         END  ;                                                         MAPS 284
              AVD ~ M + 1  ;  NUM ~ 2*(M + 1)  ;                        MAPS 285
V2:           FOR I ~ 1 STEP 1 UNTIL M DO  C[I] ~ DJ[I] ~ 0  ;          MAPS 286
V3:           IF F > B THEN F ~ B  ;                                    MAPS 287
              B ~ PM - X  ;  TAL ~ CT ~ I ~ AVC ~ 0  ;                  MAPS 288
              NUMB ~ NUMB + 1  ;                                        MAPS 289
              IF NUMB > NUM THEN                                        MAPS 290
         BEGIN                                                          MAPS 291
              WR ~ 7  ;  WRITE(OUT1,ERROR1,WR)  ;                       MAPS 292
              GO TO V5                                                  MAPS 293
         END  ;                                                         MAPS 294
              WHILE (I < M) AND (AVC < AVD) DO                          MAPS 295
         BEGIN                                                          MAPS 296
              I ~ I + 1  ;  AI ~ AB[I]  ;                               MAPS 297
              IF (AI { B) AND (C[I] < 2) THEN                           MAPS 298
         BEGIN                                                          MAPS 299
              B ~ B - AI  ;  C[I] ~ 1  ;  J ~ I  ;                      MAPS 300
              CT ~ CT + 1  ;  AVC ~ AVC + (I-AVC)/ CT                   MAPS 301
         END  ;                                                         MAPS 302
              TAL ~ TAL + C[I]                                          MAPS 303
         END  ;                                                         MAPS 304
              IF (B = 0) AND (AVC < AVD) THEN                           MAPS 305
         BEGIN                                                          MAPS 306
              AVD ~ AVC  ;                                              MAPS 307
                                                                        MAPS 308
              FOR K ~ 1 STEP 1 UNTIL M DO  DJ[K] ~ C[K]                 MAPS 309
         END  ;                                                         MAPS 310
              IF (TAL + CT) = (I+I) THEN                                MAPS 311
         BEGIN                                                          MAPS 312
              IF (F = 0) OR (B = 0) THEN GO TO V5  ;                    MAPS 313
              IF TAL = CT THEN GO TO V4  ;                              MAPS 314
              IF CT = 0 THEN BEGIN X ~ F  ;  GO TO V2  END  ;           MAPS 315
         END  ;                                                         MAPS 316
              IF B = 0 THEN NK ~ 0 ELSE NK ~ 1  ;                       MAPS 317
              FOR K ~ I STEP -1 UNTIL 1 DO                              MAPS 318
              IF C[K] = 1 THEN                                          MAPS 319
         BEGIN                                                          MAPS 320
              C[K] ~ 2  ;  NK ~ NK + 1  ;                               MAPS 321
              IF NK = 2 THEN GO TO V3                                   MAPS 322
         END                                                            MAPS 323
              ELSE C[K] ~ 0  ;                                          MAPS 324
              GO TO V3  ;                                               MAPS 325
V4:           FOR K ~ 1 STEP 1 UNTIL I DO  DJ[K] ~ 1  ;                 MAPS 326
V5:           FOR J ~ 1 STEP 1 UNTIL M DO                               MAPS 327
              IF DJ[J] = 1 THEN                                         MAPS 328
         BEGIN                                                          MAPS 329
              X ~ ODE[J]  ;  K ~ DOJOB[X]  ;  A ~ A + 1  ;              MAPS 330
              DOJOB[A] ~ K ; DOJOB[X] ~ 0 ; MENUSE ~ MENUSE + JOB[K,2]; MAPS 331
              IF ET[K] < WMIN THEN WMIN ~ ET[K]                         MAPS 332
         END                                                            MAPS 333
         END ;                                                          MAPS 334
              IF A } E THEN                                             MAPS 335
         BEGIN                                                          MAPS 336
              WR ~ 6  ;  WRITE(OUT1,ERROR1,WR)                          MAPS 337
         END ;                                                          MAPS 338
              FOR I ~ E STEP 1 UNTIL G DO                               MAPS 339
         BEGIN                                                          MAPS 340
              K ~ DOJOB[I]  ;  J ~ JOB[K,1]  ;                          MAPS 341
              IF STAR[K] ! -1 THEN                                      MAPS 342
         BEGIN                                                          MAPS 343
              IF FLAG1[K] = 0 THEN FLAG1[K] ~ CLOCK  ;                  MAPS 344
              IF J.[8:1] = 0 THEN                                       MAPS 345
         BEGIN                                                          MAPS 346
              J.[9:3] ~ J.[9:3] + 1  ;  J.[8:1] ~ 1                     MAPS 347
         END END  ;                                                     MAPS 348
              JOB[K,1] ~ J                                              MAPS 349
                                                                        MAPS 350
         END  ;                                                         MAPS 351
                                                                        MAPS 352
COMMENT       BEGIN JOBS IN DOJOB[A] FOR INCREMENT WMIN.  ;             MAPS 353
              IF WMIN = 0 THEN  BEGIN  WR ~ 8  ;  WMIN ~ 1  ;           MAPS 354
              WRITE(OUT1,ERROR1,WR)  END  ;                             MAPS 355
              NEXTIME ~ CLOCK + WMIN  ;                                 MAPS 356
              FOR I ~ 1 STEP 1 UNTIL A DO                               MAPS 357
         BEGIN                                                          MAPS 358
              K ~ DOJOB[I]  ;                                           MAPS 359
              IF (FLAG1[K] ! 0) AND (FLAG2[K]=0) THEN FLAG2[K] ~ CLOCK ;MAPS 360
              JOB[K,1].[8:1] ~ 0  ;                                     MAPS 361
              IF STAR[K] = -1 THEN STAR[K] ~ CLOCK  ;                   MAPS 362
              ET[K] ~ ET[K] - WMIN  ;                                   MAPS 363
              IF ET[K] = 0 THEN FINIS[K] ~ NEXTIME                      MAPS 364
         END ;                                                          MAPS 365
              IF SW4 THEN WRITE(OUT1,FORM3,L1)  ;                       MAPS 366
              IF MENUSE > MAXMEN THEN MAXMEN ~ MENUSE  ;                MAPS 367
              IF NEXTIME < PROJEND THEN GO TO M2  ;                     MAPS 368
              FOR I ~ 1 STEP 1 UNTIL N DO IF ET[I] ! 0 THEN GO TO M2  ; MAPS 369
              POOL ~ MAXMEN  ;                                          MAPS 370
              IF NEXTIME > PROJEND THEN                                 MAPS 371
M9:           LOW ~ POOL                                                MAPS 372
              ELSE HIGH ~ POOL  ;                                       MAPS 373
              IF SW4 THEN                                               MAPS 374
         BEGIN                                                          MAPS 375
              JM ~ NEXTIME | POOL  ;  LOW ~ POOL  ;                     MAPS 376
              WRITE(OUT1[DBL],FORM8,POOL);  WRITE(OUT1[PAGE],FORM7)  ;  MAPS 377
              FOR I ~ 1 STEP 1 UNTIL N DO                               MAPS 378
         BEGIN                                                          MAPS 379
              IF ES[I] ! STAR[I] THEN IF FLAG2[I] = 0 THEN BEGIN        MAPS 380
              FLAG2[I] ~ ES[I]  ;  Z ~ "D EST=" END                     MAPS 381
              ELSE Z ~ "  M   " ELSE Z ~ "      "  ;                    MAPS 382
              J ~ JOB[I,1].[9:3]  ;                                     MAPS 383
              IF J > 0 THEN IF J > 1 THEN Z.[30:6] ~ J                  MAPS 384
              ELSE Z.[30:6] ~ "D"  ;                                    MAPS 385
              IF Z = "      " THEN WRITE(OUT1,FORM6,L3)                 MAPS 386
              ELSE WRITE(OUT1,FORM6,L2)                                 MAPS 387
         END END                                                        MAPS 388
              ELSE                                                      MAPS 389
         BEGIN                                                          MAPS 390
              PASS ~ PASS + 1  ;  POOL ~ ENTIER((HIGH + LOW) / 2)       MAPS 391
         END  ;                                                         MAPS 392
              IF POOL = LOW THEN BEGIN SW4 ~ TRUE ;  POOL ~ HIGH-PH END;MAPS 393
              IF JM { KJ THEN BEGIN KJ ~ JM  ;  GO TO M1 END  ;         MAPS 394
         END  END .                                                     MAPS 395
                                                                        MAPS 396
