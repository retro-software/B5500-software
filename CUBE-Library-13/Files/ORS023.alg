 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                           COPT002
COMMENT       COST - PERT                                                COPT003
              PROGRAM CALCULATES "MINIMUM COST" VERSUS "DURATION" CURVE  COPT004
              ASSOCIATED WITH A GIVEN PROJECT.  CONSTITUENT ACTIVITIES   COPT005
              OF THE PROJECT ARE ACCOMPLISHED IN INCREASINGLY SHORTER    COPT006
              TIMES THEREBY PROMOTING COST.  ACTIVITIES SHORTENED ARE    COPT007
              THOSE THAT RESULT IN SMALLEST INCREASE IN COST PER UNIT    COPT008
              DECREASE IN PROJECT DURATION.  FULKERSONS FLOW METHOD IS   COPT009
              USED.                                                      COPT010
                                                                         COPT011
              W J VAN DE LINDT,   R T OLLIVIER                           COPT012
              PROFESSIONAL SERVICES DIVISIONAL GROUP                     COPT013
              BURROUGHS CORPORATION                                      COPT014
                                                                         COPT015
              CARD SEQUENCE BEGINS WITH COPT-001                         COPT016
              FIRST RELEASE  15 AUGUST, 1962 ;                           COPT017
INTEGER       K,N,Z  ;                                                   COPT018
                                                                         COPT019
INTEGER ARRAY HEAD[0:10]  ;                                              COPT020
                                                                         COPT021
FILE IN       DATA1(1,10) ;   FILE OUT   OUT1 1(1,15) ;                  COPT022
LABEL         STARTE , ENDE ;                                            COPT023
                                                                         COPT024
FORMAT IN     F1(10A6,2(X1,I3))  ;                                       COPT025
                                                                         COPT026
FORMAT OUT    F2("NETWORK NAME",X1,10A6,X1,"N",X1,I3,X1,"Z",X1,I3/)  ;   COPT027
                                                                         COPT028
LIST          L1(FOR K ~ 1 STEP 1 UNTIL 10 DO HEAD[K],N,Z)  ;            COPT029
                                                                         COPT030
STARTE :      READ (DATA1,F1,L1)[ENDE] ;  WRITE (OUT1[PAGE],F2,L1);      COPT031
         BEGIN                                                           COPT032
LABEL         START,BELA,CLEAR,LA,LA1,LA2,LA3,LA4,BR1,NOB,DONE  ;        COPT033
LABEL         ENBR ,LBL3 ;                                               COPT034
                                                                         COPT035
INTEGER       CO ,TAW1 , L, IK , JK , IP , JP , P , H ;                  COPT036
                                                                         COPT037
REAL          TAW,TAW2,TCOST  ;                                          COPT038
                                                                         COPT039
INTEGER ARRAY I,J,LL[0:N]  ;                                             COPT040
                                                                         COPT041
ARRAY        A,B,C,COST,COSTB,ET,FF,FG [0:N],ABAR1,ABAR2[0:N],TAU,       COPT042
              LM[0:N]  ;                                                 COPT043
                                                                         COPT044
LIST          L2(I[K],J[K],A[K],B[K],C[K],COSTB[K])  ,                   COPT045
              L3(K,I[K],J[K],A[K],B[K],C[K],COSTB[K])  ,                 COPT046
              L4(K,I[K],J[K],ET[K],COST[K])  ,                           COPT047
              L5(TAU[J[N]],TCOST)  ;                                     COPT048
                                                                         COPT049
FORMAT IN     F3(2(I4,X1),4(F6.0,X1))  ;                                 COPT050
                                                                         COPT051
FORMAT OUT    F4("  ACTIVITY    PREDECESSOR  SUCCESSOR   CRASH TIME  ",  COPT052
                 "NORMAL TIME  COST RATE  NORMAL COST"/)  ,              COPT053
              F5(X3,I5,X8,I5,X6,I5,2(X7,F6.0),X4,F6.0,X7,F6.0)  ,        COPT054
              F6("    ACTIVITY   PREDECESSOR SUCCESSOR   EXPECTED TIME", COPT055
                 " ACTIVITY COST" )  ,                                   COPT056
              F7(X3,I5,X8,I5,X6,I5,2(X9,F6.0))  ,                        COPT057
              F8("TOTAL PROJECT TIME  ",F6.0,"     TOTAL PROJECT COST",  COPT058
                 X3,F6.0)  ;                                             COPT059
                                                                         COPT060
START:        WRITE(OUT1[DBL],F4)  ;                                     COPT061
              Z ~ N  ;                                                   COPT062
              FOR K ~ 1 STEP 1 UNTIL N DO                                COPT063
         BEGIN                                                           COPT064
              READ(DATA1,F3,L2)  ;  WRITE(OUT1,F5,L3)                    COPT065
         END ;                                                           COPT066
              FOR K ~ 1 STEP 1 UNTIL Z DO  TAU[K] ~ 0  ;                 COPT067
              FOR K ~ 1 STEP 1 UNTIL N DO                                COPT068
         BEGIN                                                           COPT069
              TAW ~ TAU[I[K]] + B[K]  ;                                  COPT070
              IF TAW > TAU[J[K]] THEN TAU[J[K]] ~ TAW  ;                 COPT071
              FF[K] ~ FG[K] ~ 0                                          COPT072
         END ;                                                           COPT073
BELA:         FOR K ~ 1 STEP 1 UNTIL N DO                                COPT074
         BEGIN                                                           COPT075
              TAW ~ TAU[I[K]] - TAU[J[K]]  ;                             COPT076
              ABAR1[K] ~ B[K] + TAW  ;  ABAR2[K] ~ A[K] + TAW            COPT077
         END ;                                                           COPT078
CLEAR:        FOR K ~ 1 STEP 1 UNTIL Z DO  LL[K] ~ LM[K] ~ 0  ;          COPT079
              LM[I[1]] ~ 1.0 | 10 * 40  ;                                COPT080
              FOR K ~ 1 STEP 1 UNTIL N DO                                COPT081
              IF ABAR2[K] = 0 THEN IF LM[I[K]] ! 0 THEN                  COPT082
              IF LM[J[K]] = 0 THEN                                       COPT083
         BEGIN                                                           COPT084
              LL[J[K]] ~ K  ;  LM[J[K]] ~ -1.0 | 10*40                   COPT085
         END ;                                                           COPT086
              IF LM[J[N]] ! 0 THEN GO TO DONE  ;                         COPT087
              H ~ I[1] ;                                                 COPT088
              FOR K ~ 1 STEP 1 UNTIL N DO                                COPT089
         BEGIN                                                           COPT090
LBL3:         IF I[K] = H THEN                                           COPT091
         BEGIN                                                           COPT092
              IK ~ I[K]  ;  JK ~ J[K]  ;                                 COPT093
              IF LM[JK] = 0 THEN IF LM[IK] ! 0 THEN                      COPT094
         BEGIN                                                           COPT095
              IF ABAR1[K] = 0 THEN                                       COPT096
         BEGIN                                                           COPT097
              TAW ~ C[K] - FF[K]  ;                                      COPT098
              IF TAW > 0 THEN                                            COPT099
         BEGIN                                                           COPT100
              IF TAW { ABS(LM[IK]) THEN LM[JK] ~ TAW                     COPT101
              ELSE LM[JK] ~ ABS(LM[IK])  ;                               COPT102
              LL[JK] ~ K                                                 COPT103
         END  END ;                                                      COPT104
              IF ABAR2[K] = 0 THEN                                       COPT105
         BEGIN                                                           COPT106
              LM[JK] ~ -ABS(LM[IK])  ;  LL[JK] ~ K                       COPT107
         END END ;                                                       COPT108
              IF K ! N THEN GO TO LA ;                                   COPT109
              IF LM[J[N]] = 0 THEN GO TO NOB ELSE GO TO LA4              COPT110
         END ;                                                           COPT111
              FOR P ~ 1 STEP 1 UNTIL K-1 DO                              COPT112
         BEGIN                                                           COPT113
              IF J[P] = H THEN                                           COPT114
         BEGIN                                                           COPT115
              IP ~ I[P] ; JP ~ J[P] ;                                    COPT116
              IF LM[JP] ! 0 THEN IF LM[IP] = 0 THEN                      COPT117
         BEGIN                                                           COPT118
              IF ABAR1[P] ! 0 THEN GO TO LA1 ;                           COPT119
              IF FF[P] > 0 THEN                                          COPT120
         BEGIN                                                           COPT121
              IF FF[P] { ABS(LM[JP]) THEN LM[IP] ~ FF[P]                 COPT122
              ELSE LM[IP] ~ ABS(LM[JP]) ;                                COPT123
              LL[IP] ~ -P ;                                              COPT124
         END ;                                                           COPT125
LA1:          IF (ABAR2[P] ! 0 ) OR (FG[P] { 0 ) THEN GO TO LA3 ;        COPT126
              IF FG[P] { ABS(LM[JP]) THEN LM[IP] ~ -FG[P]                COPT127
              ELSE LM[IP] ~ -ABS(LM[JP]) ;                               COPT128
              LL[IP] ~ -P ;                                              COPT129
LA3:     END  END END ;                                                  COPT130
              H ~ I[K] ; GO TO LBL3 ;                                    COPT131
LA:      END ;                                                           COPT132
LA4:          TAW ~ ABS(LM[JK])  ;                                       COPT133
              TAW1 ~ LL[JK]  ;  TAW2 ~ LM[JK]  ;                         COPT134
              WHILE (TAW1 ! LL[I[1]]) DO                                 COPT135
         BEGIN                                                           COPT136
              IF TAW1 > 0 THEN                                           COPT137
         BEGIN                                                           COPT138
              IF TAW2 > 0 THEN FF[ABS(TAW1)] ~ FF[ABS(TAW1)] + TAW       COPT139
              ELSE FG[ABS(TAW1)] ~ FG[ABS(TAW1)] + TAW  ;                COPT140
              GO TO BR1                                                  COPT141
         END  ;                                                          COPT142
              IF TAW2 > 0 THEN FF[ABS(TAW1)] ~ FF[ABS(TAW1)] - TAW       COPT143
              ELSE FG[ABS(TAW1)] ~ FG[ABS(TAW1)] - TAW   ;               COPT144
BR1:          TAW2 ~ LM[I[ABS(TAW1)]]  ;                                 COPT145
              TAW1 ~ LL[I[ABS(TAW1)]]                                    COPT146
         END ;                                                           COPT147
              GO TO CLEAR  ;                                             COPT148
NOB:          TAW ~ 1.0 | 10 * 40  ;                                     COPT149
              FOR K ~ 1 STEP 1 UNTIL N DO                                COPT150
         BEGIN                                                           COPT151
              IF LM[I[K]] ! 0 THEN                                       COPT152
         BEGIN                                                           COPT153
              IF LM[J[K]] = 0 THEN                                       COPT154
         BEGIN                                                           COPT155
              IF (ABAR1[K] < 0) AND (ABS(ABAR1[K]) < TAW) THEN           COPT156
              TAW ~ ABS(ABAR1[K])  ;                                     COPT157
              IF (ABAR2[K] < 0) AND (ABS(ABAR2[K]) < TAW) THEN           COPT158
              TAW ~ ABS(ABAR2[K])                                        COPT159
              END ;                                                      COPT160
              GO TO ENBR                                                 COPT161
              END ;                                                      COPT162
              IF LM [J[K]] ! 0 THEN                                      COPT163
         BEGIN                                                           COPT164
              IF (ABAR1[K] > 0) AND (ABAR1[K] < TAW) THEN TAW ~ABAR1[K]; COPT165
              IF (ABAR2[K] > 0) AND (ABAR2[K] < TAW) THEN TAW ~ABAR2[K]  COPT166
              END ;                                                      COPT167
ENBR:         END ;                                                      COPT168
              FOR K ~ 1 STEP 1 UNTIL Z DO                                COPT169
              IF LM[K] = 0 THEN TAU[K] ~ TAU[K] - TAW  ;                 COPT170
              TCOST ~ 0  ;                                               COPT171
              FOR K ~ 1 STEP 1 UNTIL N DO                                COPT172
         BEGIN                                                           COPT173
              ET[K] ~ IF B[K] {  TAU[J[K]] - TAU[I[K]] THEN B[K]         COPT174
                      ELSE (TAU[J[K]] - TAU[I[K]])  ;                    COPT175
              COST[K] ~ COSTB[K] + C[K] | (B[K] - ET[K]) ;               COPT176
              TCOST ~ TCOST + COST[K]                                    COPT177
         END ;                                                           COPT178
              WRITE(OUT1[PAGE]);  WRITE(OUT1,F6);                        COPT179
              FOR K ~ 1 STEP 1 UNTIL N DO WRITE(OUT1[DBL],F7,L4)  ;      COPT180
              WRITE(OUT1[PAGE],F8,L5)  ;                                 COPT181
              GO TO BELA  ;                                              COPT182
DONE:         GO TO STARTE                                               COPT183
         END ;                                                           COPT184
ENDE:    END.                                                            COPT185
 LABEL  000000000DATA1  0010000001
SITE PREPARATION TAB AND KEY PUNCH ROOMS                      19  12
   1    2     0.     0.     0.     0.
   2    3     0.     0.     0.     0.
   2    4     0.     0.     0.     0.
   2    5     0.     0.     0.     0.
   3    7    40.    50.   100. 10000.
   4    6    35.    40.   100. 25000.
   5    7    10.    20.   125.  2500.
   5   11    25.    30.   100.  4000.
   6    7    15.    21.    83.  4200.
   6   10     3.     6.     0.   500.
   7    8    10.    20.   200.  5000.
   7   10     5.     5.     0.  1000.
   7   12     7.    11.   125.  3000.
   8    9     0.     0.     0.     0.
   8   10     0.     0.     0.     0.
   8   12     5.    11.    33.  2500.
   9   12     8.    10.   200.  2400.
  10   12     6.     6.     0.   750.
  11   12    15.    21.   117.  4800.
