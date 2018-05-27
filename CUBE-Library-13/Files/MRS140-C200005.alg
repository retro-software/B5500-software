         BEGIN                                                          TEST0001
COMMENT       ROOTS OF A REAL POLYNOMIAL BY THE GRAEFFE-RESULTANT METHODTEST0002
                                                                        TEST0003
              J. K. KONDO                                               TEST0004
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           TEST0005
                                                                        TEST0006
              PROCEDURE CARD SEQUENCE BEGINS WITH GRAF0001.             TEST0007
              FIRST RELEASE DATE 04-15-1964  ;                          TEST0008
                                                                        TEST0009
                                                                        TEST0010
FILE IN       CARD (1,10)  ;                                            TEST0011
FILE          PRINTER 1 (1,15)  ;                                       TEST0012
FORMAT IN     FRMT(2I3)  ;                                              TEST0013
                                                                        TEST0014
LABEL         START, EOP  ;                                             TEST0015
INTEGER       N, ALPFA  ;                                               TEST0016
START:        READ(CARD, FRMT, ALPFA, N)[EOP]  ;                        TEST0017
                                                                        TEST0018
         BEGIN                                                          TEST0019
                                                                        TEST0020
INTEGER       MP, I  ;                                                  TEST0021
REAL          DELTAP, EPSILONP  ;                                       TEST0022
ARRAY         C, GC, RE, IM, RT[0:N]  ;                                 TEST0023
INTEGER ARRAY   MU[0:N]  ;                                              TEST0024
FORMAT IN     FRMT1(11F7.2), FTIN(I3, 2E12.4)  ;                        TEST0025
FORMAT        FORM1(//X30,"ZEROS OF A REAL POLYNOMIAL, GRAEFFE METHOD"//TEST0026
              X30, "ORIGINAL COEFFICIENTS"//(X10,8F12.5)/) ,            TEST0027
              FORM3(X14, "REAL PART", X12, "IMAGINARY PART", X9,        TEST0028
              "REMAINDER TERM", X10, "MULTIPLICITY") ,                  TEST0029
              FORM4(X10, E16.8, 2E23.8, I18)  ,                         TEST0030
              FORM5(//X30, "GENERATED COEFFICIENTS"//(7E15.8))  ;       TEST0031
LIST          LIST1(FOR I := 0 STEP 1 UNTIL N DO C[I]),                 TEST0032
              LIST2(RE[I], IM[I], RT[I], MU[I]) ,                       TEST0033
              LIST3(FOR I := 0 STEP 1 UNTIL N DO GC[I])  ;              TEST0034
                                                                        TEST0035
PROCEDURE     RES(N,C,ALPFA,MU,RE,IM,RT,GC)  ;                          GRAF0001
VALUE         N, C, ALPFA  ;                                            GRAF0002
INTEGER       N, ALPFA  ;                                               GRAF0003
INTEGER ARRAY MU[0]  ;                                                  GRAF0004
ARRAY         C, RE, IM, RT, GC[0]  ;                                   GRAF0005
                                                                        GRAF0006
         BEGIN                                                          GRAF0007
                                                                        GRAF0008
COMMENT       RES FINDS SIMULTANEOUSLY ALL ZEROS OF A POLYNOMIAL OF DE- GRAF0009
              GREE N WITH REAL COEFFICIENTS, C[J] ( J = 0,...,N), WHERE GRAF0010
              C[N] IS THE CONSTANT TERM. THE REAL PART, RE[I], AND IMA- GRAF0011
              GINARY PART, IM[I], OF EACH ZERO, WITH CORRESPONDING MUL- GRAF0012
              TIPLICITY, MU[I], AND REMAINDER TERM, RT[I], (I = 1,...,  GRAF0013
              N), ARE FOUND AND A POLYNOMIAL WITH COEFFICIENTS GC[J], (JGRAF0014
              = 0,...,N), IS GENERATED FROM THESE ZEROS. ALPFA PROVIDES GRAF0015
              AN OPTION FOR LOCAL OR NONLOCAL SELECTION OF M, THE NUMBERGRAF0016
              OF ROOT-SQUARING OPERATIONS, AND DELTA AND EPSILON, ACCEP-GRAF0017
              TANCE CRITERIA. IF ALPFA = 1, THESE PARAMETERS ARE ASSIGN-GRAF0018
              ED LOCALLY. IF ALPFA = 2, M, DELTA AND EPSILON ARE SET    GRAF0019
              EQUAL TO THE GLOBAL PARAMETERS, MP, DELTAP, AND EPSILONP, GRAF0020
              RESPECTIVELY. IN CASES WHERE ZEROS MAY BE FOUND MORE THAN GRAF0021
              ONCE, THE SUPERFLUOUS ONES ARE ELIMINATED BY FACTORIZATIONGRAF0022
              ;                                                         GRAF0023
                                                                        GRAF0024
INTEGER       M  ;                                                      GRAF0025
REAL          DELTA, EPSILON  ;                                         GRAF0026
LABEL         START, U1, U2  ;                                          GRAF0027
SWITCH        U := U1, U2  ;                                            GRAF0028
                                                                        GRAF0029
              GO TO U[ALPFA]  ;                                         GRAF0030
U1:           M := 10  ;  DELTA := 0.2  ;  EPSILON := 1@-8  ;           GRAF0031
              GO TO START  ;                                            GRAF0032
U2:           M := MP  ;  DELTA := DELTAP  ;  EPSILON := EPSILONP  ;    GRAF0033
                                                                        GRAF0034
START:   BEGIN                                                          GRAF0035
                                                                        GRAF0036
INTEGER       CT, NU, NUC, BETA, SM, J, JC, K, I, P, M1, MC  ;          GRAF0037
BOOLEAN       ROOT  ;                                                   GRAF0038
REAL          X, Y, GX, RP  ;                                           GRAF0039
LABEL         SQUAROP, RD, ONE, TWO, THREE, T1, T2, S1, S2, MULT, IT,   GRAF0040
              V1, V2, E, D, RESULTANT  ;                                GRAF0041
ARRAY         A[0:N,0:M], AC, R, RC, T[0:N], S, AG[-1:N],               GRAF0042
              RH, Q, G, F[1:2|N]  ;                                     GRAF0043
SWITCH        SC := S1, S2  ;                                           GRAF0044
SWITCH        TC := T1, T2  ;                                           GRAF0045
SWITCH        V := V1, V2  ;                                            GRAF0046
                                                                        GRAF0047
REAL PROCEDURE  SYND(W, Q, I, T, S)  ;                                  GRAF0048
VALUE         W, Q, I  ;                                                GRAF0049
INTEGER       I  ;                                                      GRAF0050
REAL          W, Q  ;                                                   GRAF0051
ARRAY         S[-1], T[0]  ;                                            GRAF0052
                                                                        GRAF0053
         BEGIN                                                          GRAF0054
                                                                        GRAF0055
INTEGER       M  ;                                                      GRAF0056
              S[-1] := 0  ;  S[0] := T[0]  ;                            GRAF0057
              FOR M := 1 STEP 1 UNTIL I DO                              GRAF0058
              S[M] := T[M] - S[M-1]|W - S[M-2]|Q  ;                     GRAF0059
              IF Q ! 0 AND I ! 1 THEN SYND := ABS(S[I-1]|W|0.5 + S[I])  GRAF0060
              ELSE SYND := ABS(S[I])                                    GRAF0061
         END  ;                                                         GRAF0062
                                                                        GRAF0063
              CT := BETA := 1  ;  ROOT := FALSE  ;                      GRAF0064
              FOR J := 0 STEP 1 UNTIL N DO A[J,0] := C[J]  ;            GRAF0065
                                                                        GRAF0066
SQUAROP: BEGIN                                                          GRAF0067
COMMENT       PERFORM ROOT-SQUARING OPERATION  ;                        GRAF0068
INTEGER       EL  ;                                                     GRAF0069
REAL          H  ;                                                      GRAF0070
LABEL          EXIT  ;                                                  GRAF0071
              M1 := M  ;                                                GRAF0072
                                                                        GRAF0073
              FOR SM := 1 STEP 1 UNTIL M1 DO                            GRAF0074
         BEGIN                                                          GRAF0075
              FOR J := 0 STEP 1 UNTIL N DO                              GRAF0076
         BEGIN                                                          GRAF0077
              H := 0  ;                                                 GRAF0078
              IF N-J { J THEN K := N-J ELSE K := J  ;                   GRAF0079
              FOR EL := 1 STEP 1 UNTIL K DO                             GRAF0080
              H := (-1)*EL | A[J-EL,SM-1] | A[J+EL,SM-1] + H  ;         GRAF0081
              A[J,SM] := (-1)*J | (A[J,SM-1]*2 + 2|H)                   GRAF0082
         END  ;                                                         GRAF0083
COMMENT       A TEST IS MADE ON THE MAGNITUDE OF THE COEFFICIENTS OF THEGRAF0084
              SM-TH ITERATION.   ;                                      GRAF0085
              FOR J := 0 STEP 1 UNTIL N DO                              GRAF0086
              IF A[J,SM] ! 0 THEN                                       GRAF0087
              IF ABS(A[J,SM]) > 1.0@32 OR ABS(A[J,SM]) < 1.0@-28 THEN   GRAF0088
         BEGIN                                                          GRAF0089
              M1 := SM  ;  GO TO EXIT                                   GRAF0090
         END                                                            GRAF0091
         END  ;                                                         GRAF0092
                                                                        GRAF0093
EXIT:    END SQUARING OPERATION;                                        GRAF0094
                                                                        GRAF0095
COMMENT       DETERMINE PIVOTAL COEFFICIENTS  ;                         GRAF0096
              FOR J := 1 STEP 1 UNTIL N-1 DO                            GRAF0097
              R[J] := IF A[J,M1] ! 0 THEN (-1)*J | A[J,M1-1]*2 / A[J,M1]GRAF0098
              ELSE IF A[J,M1-1] = 0 THEN 1.0 ELSE 0  ;                  GRAF0099
              R[N] := 1   ;                                             GRAF0100
              J := NU := 1  ;                                           GRAF0101
                                                                        GRAF0102
RD:           IF ABS(R[J] - 1) { DELTA THEN                             GRAF0103
         BEGIN                                                          GRAF0104
              RP := (A[J,M1] / A[J-NU,M1]) * (1.0 / (2*M1 | NU))  ;     GRAF0105
              GO TO TC[BETA]                                            GRAF0106
         END  ;                                                         GRAF0107
                                                                        GRAF0108
ONE:          NU := NU + 1  ;                                           GRAF0109
TWO:          J := J + 1  ;                                             GRAF0110
              IF J = N+1 THEN GO TO SC[BETA]                            GRAF0111
              ELSE GO TO RD  ;                                          GRAF0112
THREE:        NU := 1  ;  GO TO TWO  ;                                  GRAF0113
                                                                        GRAF0114
COMMENT       TEST WHETHER (X-RP) IS A LINEAR FACTOR OF THE POLYNOMIAL ;GRAF0115
T1:           X := RP | EPSILON + RP  ;  Y := RP | EPSILON + X  ;       GRAF0116
              FOR K := 0 STEP 1 UNTIL N DO T[K] := ABS(C[K])  ;         GRAF0117
              F[CT] := SYND(-Y,0,N,T,S) - SYND(-X,0,N,T,S)  ;           GRAF0118
                                                                        GRAF0119
              FOR RH[CT] := RP, -RP DO                                  GRAF0120
         BEGIN                                                          GRAF0121
              G[CT] := SYND(RH[CT], 0, N, C, S)  ;                      GRAF0122
              IF F[CT] > G[CT] THEN                                     GRAF0123
         BEGIN                                                          GRAF0124
              ROOT := TRUE  ;                                           GRAF0125
              Q[CT] := 0  ;                                             GRAF0126
              CT := CT + 1  ;                                           GRAF0127
              F[CT] := F[CT-1]                                          GRAF0128
         END                                                            GRAF0129
         END  ;                                                         GRAF0130
                                                                        GRAF0131
              IF NU = 1 THEN GO TO TWO  ;                               GRAF0132
              Q[CT] := RP*2  ;                                          GRAF0133
              NUC := NU  ;  JC := J  ;  MC := M1  ;                     GRAF0134
              FOR J := 0 STEP 1 UNTIL N DO                              GRAF0135
         BEGIN                                                          GRAF0136
              RC[J] := R[J]  ;  AC[J] := A[J,M1]                        GRAF0137
         END  ;                                                         GRAF0138
                                                                        GRAF0139
RESULTANT: BEGIN                                                        GRAF0140
COMMENT       THE RESULTANT OF THE ORIGINAL POLYNOMIAL AND (X*2 + RP|X  GRAF0141
              + Q) IS COMPUTED  ;                                       GRAF0142
INTEGER       L, COUNT  ;                                               GRAF0143
REAL          LA, H  ;                                                  GRAF0144
ARRAY         CB[-1:N+1], RS[0:N,0:N], B[0:N,-1:N]  ;                   GRAF0145
              FOR K := 1 STEP 2 UNTIL N-1 DO B[K,-1] := 0  ;            GRAF0146
              CB[-1] := CB[N+1] := 0  ;                                 GRAF0147
              FOR J := 0 STEP 1 UNTIL N DO CB[J] := C[J]  ;             GRAF0148
              COUNT := 1  ;                                             GRAF0149
                                                                        GRAF0150
              FOR K := 0 STEP 1 UNTIL N DO                              GRAF0151
         BEGIN                                                          GRAF0152
              B[K,K] := 1.0  ;                                          GRAF0153
              L := IF COUNT = 1 THEN 0 ELSE 1  ;                        GRAF0154
              FOR J := L STEP 2 UNTIL K-2 DO                            GRAF0155
              B[K,J] := B[K-1,J-1] - Q[CT] | B[K-2,J]  ;                GRAF0156
              H := 0  ;                                                 GRAF0157
              FOR J := N-K STEP -1 UNTIL 0 DO                           GRAF0158
              H := (CB[J] | CB[K+J] - CB[J-1] | CB[K+J+1])              GRAF0159
              | Q[CT]*(N-K-J) + H  ;                                    GRAF0160
              LA := COUNT | H  ;                                        GRAF0161
              FOR J := L STEP 2 UNTIL K-2 DO                            GRAF0162
              RS[K,J] := B[K,J] | LA + RS[K-2,J]  ;                     GRAF0163
              RS[K,K] := LA  ;                                          GRAF0164
              COUNT := -COUNT                                           GRAF0165
         END  ;                                                         GRAF0166
                                                                        GRAF0167
              FOR J := N-1 STEP -2 UNTIL 0 DO                           GRAF0168
              RS[N,J] := RS[N-1,J]  ;                                   GRAF0169
              BETA := 2  ;                                              GRAF0170
              FOR J := 0 STEP 1 UNTIL N DO                              GRAF0171
              A[J,0] := RS[N,N-J]  ;                                    GRAF0172
              GO TO SQUAROP                                             GRAF0173
         END  ;                                                         GRAF0174
                                                                        GRAF0175
COMMENT       TEST WHETHER (X*2 + RP|X + Q) IS A QUADRATIC FACTOR OF    GRAF0176
              THE POLYNOMIAL  ;                                         GRAF0177
T2:           IF (RP/2)*2 } Q[CT] THEN GO TO THREE  ;                   GRAF0178
                                                                        GRAF0179
              FOR RH[CT] := RP, -RP DO                                  GRAF0180
         BEGIN                                                          GRAF0181
              G[CT] := SYND(RH[CT],Q[CT],N,C,S)  ;                      GRAF0182
              IF F[CT] > G[CT] THEN                                     GRAF0183
         BEGIN                                                          GRAF0184
              CT := CT + 1  ;                                           GRAF0185
              F[CT] := F[CT-1]  ;                                       GRAF0186
              Q[CT] := Q[CT-1]  ;                                       GRAF0187
         END                                                            GRAF0188
         END  ;                                                         GRAF0189
              GO TO THREE  ;                                            GRAF0190
                                                                        GRAF0191
COMMENT       RESTORE ORIGINAL VALUES OF A[,], R[], NU, J, M  ;         GRAF0192
S2:           FOR J := 0 STEP 1 UNTIL N DO                              GRAF0193
         BEGIN                                                          GRAF0194
              A[J,M1] := AC[J]  ;                                       GRAF0195
              R[J] := RC[J]                                             GRAF0196
         END  ;                                                         GRAF0197
              J := JC  ;  BETA := 1  ;  M1 := MC  ;                     GRAF0198
              IF ROOT THEN GO TO THREE  ;                               GRAF0199
              NU := NUC  ;                                              GRAF0200
              GO TO ONE  ;                                              GRAF0201
                                                                        GRAF0202
COMMENT       OUTPUT ROUTINE AND DETERMINATION OF MULTIPLICITY  ;       GRAF0203
S1:           AG[-1] := 0  ;  AG[0] := C[0]  ;                          GRAF0204
              FOR J := 1 STEP 1 UNTIL N DO AG[J] := 0  ;                GRAF0205
              SM := K := 1  ;  I := N  ;                                GRAF0206
              FOR J := 0 STEP 1 UNTIL N DO T[J] := C[J]  ;              GRAF0207
MULT:         MU[SM] := 0  ;                                            GRAF0208
              P := IF Q[K] = 0 THEN 1 ELSE 2  ;                         GRAF0209
IT:           GX := SYND(RH[K],Q[K],I,T,S)  ;                           GRAF0210
                                                                        GRAF0211
              IF F[K] > GX THEN                                         GRAF0212
         BEGIN                                                          GRAF0213
              FOR J := N STEP -1 UNTIL 1 DO                             GRAF0214
              AG[J] := RH[K] | AG[J-1] + Q[K] | AG[J-2] + AG[J]  ;      GRAF0215
              MU[SM] := MU[SM] + P  ;   I := I - P  ;                   GRAF0216
              FOR J := 0 STEP 1 UNTIL I DO T[J] := S[J]  ;              GRAF0217
              GO TO IT                                                  GRAF0218
         END                                                            GRAF0219
              ELSE IF MU[SM] ! 0 THEN                                   GRAF0220
         BEGIN                                                          GRAF0221
              RT[SM] := G[K]  ;                                         GRAF0222
              GO TO V[P]                                                GRAF0223
         END                                                            GRAF0224
              ELSE GO TO D  ;                                           GRAF0225
                                                                        GRAF0226
V1:           RE[SM] := -RH[K]  ;  IM[SM] := 0  ;  GO TO E  ;           GRAF0227
V2:           RE[SM] := -RH[K]/2  ;  IM[SM] := SQRT(-RE[SM]*2 + Q[K])  ;GRAF0228
E:            SM := SM + 1  ;                                           GRAF0229
D:            K := K + 1  ;                                             GRAF0230
              IF K { CT AND SM { N THEN GO TO MULT  ;                   GRAF0231
              FOR J := 0 STEP 1 UNTIL N DO GC[J] := AG[J]               GRAF0232
         END                                                            GRAF0233
         END  ;                                                         GRAF0234
                                                                        TEST0036
              IF ALPFA = 2 THEN READ(CARD, FTIN, MP, DELTAP, EPSILONP) ;TEST0037
              READ(CARD, FRMT1, LIST1)  ;                               TEST0038
              WRITE(PRINTER, FORM1, LIST1)  ;                           TEST0039
              RES(N, C, ALPFA, MU, RE, IM, RT, GC)  ;                   TEST0040
              WRITE(PRINTER,FORM3)  ;                                   TEST0041
              FOR I := 1 STEP 1 UNTIL N DO WRITE(PRINTER,FORM4,LIST2)  ;TEST0042
              WRITE(PRINTER, FORM5, LIST3)  ;                           TEST0043
         END  ;                                                         TEST0044
              GO TO START  ;                                            TEST0045
EOP:     END  .                                                         TEST0046
