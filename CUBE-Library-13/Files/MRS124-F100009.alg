PROCEDURE COMPMATMUL(M, N, P, A, IA, B, IB, C, IC) ;                    CMXM0001
                                                                        CMXM0002
VALUE         M, N, P ;                                                 CMXM0003
INTEGER       M, N, P ;                                                 CMXM0004
REAL ARRAY    A, IA, B, IB, C, IC[*,*] ;                                CMXM0005
                                                                        CMXM0006
COMMENT       THIS PROCEDURE MULTIPLIES MATRICES WITH COMPLEX ELEMENTS. CMNT0002
              SEE CORREPSONDING TECHNICAL BULLETIN FOR DETAILS ON USE   CMNT0003
              OF PROCEDURE.                                             CMNT0004
                                                                        CMNT0005
              R.D. RODMAN                                               CMNT0006
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),                 CMNT0007
                                                                        CMNT0008
              CARD SEQUENCE BEGINS WITH CMXM0001,                       CMNT0009
              FIRST RELEASE 4/1/63 ;                                    CMNT0010
                                                                        CMXM0007
         BEGIN                                                          CMXM0008
INTEGER       I, J, K;                                                  CMXM0009
REAL ARRAY    Q1, Q2[0:1,0:IF M}N AND M}P THEN M ELSE IF N}M AND N}P    CMXM0010
              THEN N ELSE P] ;                                          CMXM0011
PROCEDURE     CIP(A, B, N) ;                                            CMXM0012
                                                                        CMXM0013
VALUE         N ;                                                       CMXM0014
INTEGER       N ;                                                       CMXM0015
ARRAY         A, B[0,0] ;                                               CMXM0016
                                                                        CMXM0017
         BEGIN                                                          CMXM0018
REAL          Q, IQ ;                                                   CMXM0019
INTEGER       I ;                                                       CMXM0020
                                                                        CMXM0021
              Q ~ IQ ~ 0 ;                                              CMXM0022
                                                                        CMXM0023
              FOR I ~ 1 STEP 1 UNTIL N DO                               CMXM0024
         BEGIN                                                          CMXM0025
              Q ~ A[0,I] | B[0,I] - A[1,I] | B[1,I] + Q ;               CMXM0026
              IQ ~ A[1,I] | B[0,I] + A[0,I] | B[1,I] + IQ               CMXM0027
         END ;                                                          CMXM0028
              A[0,0] ~ Q ;  A[1,0] ~ IQ                                 CMXM0029
         END ;                                                          CMXM0030
                                                                        CMXM0031
              FOR I ~ 1 STEP 1 UNTIL M DO                               CMXM0032
         BEGIN                                                          CMXM0033
                                                                        CMXM0034
              FOR K ~ 1 STEP 1 UNTIL N DO                               CMXM0035
         BEGIN                                                          CMXM0036
              Q1[0,K] ~ A[I,K] ;  Q1[1,K] ~ IA[I,K]                     CMXM0037
         END ;                                                          CMXM0038
                                                                        CMXM0039
              FOR J ~ 1 STEP 1 UNTIL P DO                               CMXM0040
         BEGIN                                                          CMXM0041
                                                                        CMXM0042
              FOR K ~ 1 STEP 1 UNTIL N DO                               CMXM0043
         BEGIN                                                          CMXM0044
              Q2[0,K] ~ B[K,J] ;  Q2[1,K] ~ IB[K,J]                     CMXM0045
         END ;                                                          CMXM0046
              CIP(Q1, Q2, N) ;                                          CMXM0047
              C[I,J] ~ Q1[0,0] ;  IC[I,J] ~ Q1[1,0]                     CMXM0048
         END                                                            CMXM0049
         END                                                            CMXM0050
         END ;                                                          CMXM0051
PROCEDURE COMPINV(N, A, IA, EPS, SINGULAR) ;                            CINV0001
                                                                        CINV0002
VALUE         N, EPS ;                                                  CINV0003
INTEGER       N ;                                                       CINV0004
REAL          EPS ;                                                     CINV0005
ARRAY         A, IA[0,0] ;                                              CINV0006
LABEL         SINGULAR ;                                                CINV0007
                                                                        CINV0008
COMMENT       THIS PROCEDURE INVERTS A MATRIX OF COMPLEX ELEMENTS.      CMNT0002
              SEE CORRESPONDING TECHNICAL BULLETIN FOR DETAILS  ON USE  CMNT0003
              OF THE PROCEDURE.                                         CMNT0004
                                                                        CMNT0005
              R.D. RODMAN                                               CMNT0006
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),                 CMNT0007
                                                                        CMNT0008
              CARD SEQUENCE BEGINS WITH CINV0001,                       CMNT0009
              FIRST RELEASE 4/1/63 ;                                    CMNT0010
                                                                        CINV0009
         BEGIN                                                          CINV0010
INTEGER       I, Z, K, L, K2, J, V, Y, W ;                              CINV0011
REAL          BIG, T, EPS, TEMP, DIAG, IT ;                             CINV0012
ARRAY         Q1[0:1,0:N], Q2[0:1,0:N] ;                                CINV0013
INTEGER ARRAY F[0:N] ;                                                  CINV0014
PROCEDURE     CIP(A, B, N) ;                                            CINV0015
                                                                        CINV0016
VALUE         N ;                                                       CINV0017
INTEGER       N ;                                                       CINV0018
                                                                        CINV0020
         BEGIN                                                          CINV0021
ARRAY         A, B[0,0] ;                                               CINV0019
REAL          Q, IQ ;                                                   CINV0022
INTEGER       I ;                                                       CINV0023
                                                                        CINV0024
              Q ~ IQ ~ 0 ;                                              CINV0025
                                                                        CINV0026
              FOR I ~ 1 STEP 1 UNTIL N DO                               CINV0027
         BEGIN                                                          CINV0028
              Q ~ A[0,I] | B[0,I] - A[1,I] | B[1,I] + Q ;               CINV0029
              IQ ~ A[1,I] | B[0,I] + A[0,I] | B[1,I] + IQ               CINV0030
         END ;                                                          CINV0031
              A[0,0] ~ Q ;  A[1,0] ~ IQ                                 CINV0032
         END ;                                                          CINV0033
                                                                        CINV0034
              FOR I ~ 1 STEP 1 UNTIL N DO                               CINV0035
         BEGIN                                                          CINV0036
              Z ~ I-1 ;                                                 CINV0037
                                                                        CINV0038
              FOR K ~ 1 STEP 1 UNTIL Z DO                               CINV0039
         BEGIN                                                          CINV0040
              Q1[0,K] ~ A[K,I] ;  Q1[1,K] ~ IA[K,I]                     CINV0041
         END ;                                                          CINV0042
                                                                        CINV0043
              FOR K ~ I STEP 1 UNTIL N DO                               CINV0044
         BEGIN                                                          CINV0045
                                                                        CINV0046
              FOR L ~ 1 STEP 1 UNTIL Z DO                               CINV0047
         BEGIN                                                          CINV0048
              Q2[0,L] ~ A[K,L] ;  Q2[1,L] ~ IA[K,L]                     CINV0049
         END ;                                                          CINV0050
              CIP(Q1, Q2, Z) ;                                          CINV0051
              A[K,I] ~ A[K,I] - Q1[0,0] ;  IA[K,I] ~ IA[K,I] - Q1[1,0]  CINV0052
         END ;                                                          CINV0053
              BIG ~ 0 ;  K2 ~ I ;                                       CINV0054
                                                                        CINV0055
              FOR K ~ I STEP 1 UNTIL N DO                               CINV0056
         BEGIN                                                          CINV0057
              T ~ A[K,I]*2 + IA[K,I]*2 ;                                CINV0058
              IF T > BIG THEN                                           CINV0059
         BEGIN                                                          CINV0060
              BIG ~ T ;  K2 ~ K                                         CINV0061
         END                                                            CINV0062
         END ;                                                          CINV0063
              IF BIG { EPS THEN GO TO SINGULAR ;                        CINV0064
              F[I] ~ K2 ;                                               CINV0065
              IF K2 ! I THEN FOR K ~ 1 STEP 1 UNTIL N DO                CINV0066
         BEGIN                                                          CINV0067
              TEMP ~ A[I,K] ;  A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP ;     CINV0068
              TEMP ~ IA[I,K] ;  IA[I,K] ~ IA[K2,K] ;  IA[K2,K] ~ TEMP   CINV0069
         END ;                                                          CINV0070
              DIAG ~ 1/(A[I,I]*2 + IA[I,I]*2) ;                         CINV0071
                                                                        CINV0072
              FOR K ~ 1 STEP 1 UNTIL Z DO                               CINV0073
         BEGIN                                                          CINV0074
              Q1[0,K] ~ A[I,K] ;  Q1[1,K] ~ IA[I,K]                     CINV0075
         END ;                                                          CINV0076
                                                                        CINV0077
              FOR K ~ I+1 STEP 1 UNTIL N DO                             CINV0078
         BEGIN                                                          CINV0079
                                                                        CINV0080
              FOR L ~ 1 STEP 1 UNTIL Z DO                               CINV0081
         BEGIN                                                          CINV0082
              Q2[0,L] ~ A[L,K] ;  Q2[1,L] ~ IA[L,K]                     CINV0083
         END ;                                                          CINV0084
              CIP(Q1, Q2, Z) ;                                          CINV0085
              T ~ A[I,K] - Q1[0,0] ;  IT ~ IA[I,K] - Q1[1,0] ;          CINV0086
              A[I,K] ~(T|A[I,I] + IT|IA[I,I]) | DIAG ;                  CINV0087
              IA[I,K] ~ (IT|A[I,I] - T|IA[I,I]) | DIAG                  CINV0088
         END                                                            CINV0089
         END ;                                                          CINV0090
                                                                        CINV0091
              FOR I ~ 1 STEP 1 UNTIL N DO                               CINV0092
         BEGIN                                                          CINV0093
              DIAG ~ 1/(A[I,I]*2 + IA[I,I]*2) ;   Z ~ I-1 ;             CINV0094
                                                                        CINV0095
              FOR J ~ 1 STEP 1 UNTIL I DO                               CINV0096
         BEGIN                                                          CINV0097
              IF I ! J THEN                                             CINV0098
         BEGIN                                                          CINV0099
                                                                        CINV0100
              FOR K ~ J STEP 1 UNTIL Z DO                               CINV0101
         BEGIN                                                          CINV0102
              Q1[0,K-J+1] ~ A[K,J] ;  Q1[1,K-J+1] ~ IA[K,J] ;           CINV0103
              Q2[0,K-J+1] ~ A[I,K] ;  Q2[1,K-J+1] ~ IA[I,K]             CINV0104
         END ;                                                          CINV0105
              CIP(Q1, Q2, I-J) ;                                        CINV0106
              A[I,J] ~ (-Q1[0,0]|A[I,I] - Q1[1,0]|IA[I,I]) | DIAG ;     CINV0107
              IA[I,J] ~ (Q1[0,0]|IA[I,I]- Q1[1,0]|A[I,I]) |DIAG         CINV0108
         END                                                            CINV0109
              ELSE                                                      CINV0110
         BEGIN                                                          CINV0111
              A[I,I] ~ A[I,I] | DIAG ;                                  CINV0112
              IA[I,I] ~ -IA[I,I] | DIAG                                 CINV0113
         END                                                            CINV0114
         END                                                            CINV0115
         END ;                                                          CINV0116
              V ~ N-1 ;                                                 CINV0117
                                                                        CINV0118
              FOR I ~ V STEP -1 UNTIL 1 DO                              CINV0119
         BEGIN                                                          CINV0120
              Z ~ I+1 ;                                                 CINV0121
                                                                        CINV0122
              FOR J ~ N STEP -1 UNTIL Z DO                              CINV0123
         BEGIN                                                          CINV0124
              Y ~ J-1 ;                                                 CINV0125
                                                                        CINV0126
              FOR K ~ I+1 STEP 1 UNTIL Y DO                             CINV0127
         BEGIN                                                          CINV0128
              Q1[0,W~K-I] ~ A[K,J] ;  Q1[1,W] ~ IA[K,J] ;               CINV0129
              Q2[0,W] ~ A[I,K] ;  Q2[1,W] ~ IA[I,K]                     CINV0130
         END ;                                                          CINV0131
              CIP(Q1, Q2, Y-I) ;                                        CINV0132
              A[I,J] ~ -A[I,J] - Q1[0,0] ;                              CINV0133
              IA[I,J] ~ -IA[I,J] - Q1[1,0]                              CINV0134
         END                                                            CINV0135
         END ;                                                          CINV0136
                                                                        CINV0137
              FOR I ~ 1 STEP 1 UNTIL V DO                               CINV0138
         BEGIN                                                          CINV0139
                                                                        CINV0140
              FOR J ~ 1 STEP 1 UNTIL N DO                               CINV0141
         BEGIN                                                          CINV0142
              IF I } J THEN                                             CINV0143
         BEGIN                                                          CINV0144
                                                                        CINV0145
              FOR K ~ I+1 STEP 1 UNTIL N DO                             CINV0146
         BEGIN                                                          CINV0147
              Q1[0,K-I] ~ A[I,K] ;  Q1[1,K-I] ~ IA[I,K] ;               CINV0148
              Q2[0,K-I] ~ A[K,J] ;  Q2[1,K-I] ~ IA[K,J]                 CINV0149
         END ;                                                          CINV0150
              CIP(Q1, Q2, N-I) ;                                        CINV0151
              A[I,J] ~ A[I,J] + Q1[0,0] ;                               CINV0152
              IA[I,J] ~ IA[I,J] + Q1[1,0]                               CINV0153
         END                                                            CINV0154
              ELSE                                                      CINV0155
         BEGIN                                                          CINV0156
                                                                        CINV0157
              FOR K ~ J STEP 1 UNTIL N DO                               CINV0158
         BEGIN                                                          CINV0159
              Q1[0,W~K-J+1] ~ A[K,J] ;  Q1[1,W] ~ IA[K,J] ;             CINV0160
              Q2[0,W] ~ A[I,K] ;  Q2[1,W] ~ IA[I,K]                     CINV0161
         END ;                                                          CINV0162
              CIP(Q1, Q2, N-J+1) ;                                      CINV0163
              A[I,J] ~ Q1[0,0] ;  IA[I,J] ~ Q1[1,0]                     CINV0164
         END                                                            CINV0165
         END                                                            CINV0166
         END ;                                                          CINV0167
                                                                        CINV0168
              FOR J ~ N STEP -1 UNTIL 1 DO                              CINV0169
         BEGIN                                                          CINV0170
              IF F[J] ! J THEN                                          CINV0171
         BEGIN                                                          CINV0172
              K2 ~ F[J] ;                                               CINV0173
                                                                        CINV0174
              FOR K ~ 1 STEP 1 UNTIL N DO                               CINV0175
         BEGIN                                                          CINV0176
              TEMP ~ A[K,K2] ;  A[K,K2] ~ A[K,J] ;  A[K,J] ~ TEMP ;     CINV0177
              TEMP ~ IA[K,K2] ;  IA[K,K2] ~ IA[K,J] ;  IA[K,J] ~ TEMP   CINV0178
         END                                                            CINV0179
         END                                                            CINV0180
              ELSE                                                      CINV0181
         END                                                            CINV0182
         END ;                                                          CINV0183

