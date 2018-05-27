PROCEDURE     PHIPART (V, DATA, ALEPH, BETA, COEFF, P, S, PHI, SUM) ;   SFSB0001
                                                                        SFSB0002
COMMENT       PROCEDURE PHIPART FOR CALCULATING THE VALUE OF THE        SFSB0003
              DEPENDENT VARIABLE AND ITS PARTIAL DERIVATIVES FROM       SFSB0004
              A SURFACE FIT IN ORTHOGONAL POLYNOMIALS                   SFSB0005
              BY EDWIN B. SEIDMAN                                       SFSB0006
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)      SFSB0007
              CARD SEQUENCE STARTS WITH SFSB0001                        SFSB0008
              FIRST RELEASE DATE : 7-15-64                              SFSB0009
                                                                        SFSB0010
              INPUT PARAMETERS :                                        SFSB0011
                   V  -  NUMBER OF INDEPENDENT VARIABLES                SFSB0012
                   DATA - ORDERED SET OF VALUES OF THE INDEPENDENT      SFSB0013
                          VARIABLES, REAL ARRAY                         SFSB0014
                   ALEPH - ALPHA COEFFICIENTS FROM SURFACE FIT, REAL    SFSB0015
                           ARRAY                                        SFSB0016
                   BETA - BETA COEFFICIENTS FROM SURFACE FIT, REAL      SFSB0017
                          ARRAY                                         SFSB0018
                   P - NUMBER OF ORTHOGONAL POLYNOMIALS IN EACH         SFSB0019
                       INDEPENDENT VARIABLE, INTEGER ARRAY              SFSB0020
                   S - CALLS FOR PARTIAL DERIVATIVES, BOOLEAN ARRAY     SFSB0021
                                                                        SFSB0022
              OUTPUT PARAMETERS :                                       SFSB0023
                   PHI - VALUE OF DEPENDENT VARIABLE, REAL              SFSB0024
                   SUM - ORDERED SET OF PARTIAL DERIVATIVES  ;          SFSB0025
                                                                        SFSB0026
VALUE         V ;                                                       SFSB0027
INTEGER       V ;                                                       SFSB0028
INTEGER ARRAY P[0] ;                                                    SFSB0029
BOOLEAN ARRAY S[0] ;                                                    SFSB0030
REAL ARRAY    DATA[0], ALEPH[0,0], BETA[0,0], COEFF[0], SUM[0] ;        SFSB0031
REAL          PHI ;                                                     SFSB0032
                                                                        SFSB0033
         BEGIN                                                          SFSB0034
DEFINE        TADD = TI1S1 #, ADD = TI1S2 #, DEGREE = TI1S3 #,          SFSB0035
              POLY = TR1S1 #, TT = TR1S2 # ;                            SFSB0036
INTEGER       KV, M, T, I, D, PV ;                                      SFSB0037
BOOLEAN       B ;                                                       SFSB0038
REAL          DATUM ;                                                   SFSB0039
LABEL         POLYCOMPUTE, PARTCOMPUTE, D2, BACK, HERE, FINI, DEPENDENT;SFSB0040
                                                                        SFSB0041
              TADD[1] ~ ADD[1] ~ T ~ 1 ;                                SFSB0042
                                                                        SFSB0043
              FOR M ~ 1 STEP 1 UNTIL V DO                               SFSB0044
         BEGIN                                                          SFSB0045
              DATUM ~ DATA[M] ;          SUM[M] ~ 0 ;                   SFSB0046
                                                                        SFSB0047
COMMENT       EVALUATE THE POLYNOMIALS ;                                SFSB0048
                                                                        SFSB0049
POLYCOMPUTE :                                                           SFSB0050
         BEGIN                                                          SFSB0051
              POLY[T] ~ 1.0 ;                                           SFSB0052
              POLY[T+1] ~ DATUM + ALEPH[M,1] ;          T ~ T + 2 ;     SFSB0053
                                                                        SFSB0054
              FOR I ~ 2 STEP 1 UNTIL P[M] DO                            SFSB0055
         BEGIN                                                          SFSB0056
              POLY[T] ~ (ALEPH[M,I] + DATUM) | POLY[T-1] +              SFSB0057
              BETA[M,I] | POLY[T-2] ;                                   SFSB0058
              T ~ T + 1                                                 SFSB0059
         END                                                            SFSB0060
         END POLYCOMPUTE ;                                              SFSB0061
              TADD[M+1] ~ ADD[M+1] ~ T                                  SFSB0062
         END ;                                                          SFSB0063
              B ~ FALSE ;                                               SFSB0064
                                                                        SFSB0065
COMMENT       COMPUTE THE DEPENDENT VARIABLE OR THE PARTIAL ;           SFSB0066
                                                                        SFSB0067
DEPENDENT :                                                             SFSB0068
         BEGIN                                                          SFSB0069
              D ~ 1 ;                                                   SFSB0070
                                                                        SFSB0071
              FOR M ~ 1 STEP 1 UNTIL V DO                               SFSB0072
         BEGIN                                                          SFSB0073
              DEGREE[M] ~ TADD[M] ;          TT[M] ~ 0                  SFSB0074
         END ;                                                          SFSB0075
              PV ~ TADD[V] + P[V] ;                                     SFSB0076
                                                                        SFSB0077
BACK :        FOR I ~ TADD[V] STEP 1 UNTIL PV DO                        SFSB0078
         BEGIN                                                          SFSB0079
              TT[V] ~ COEFF[D] | POLY[I] + TT[V] ;                      SFSB0080
              D ~ D + 1                                                 SFSB0081
         END ;                                                          SFSB0082
              IF V = 1 THEN GO TO FINI ;                                SFSB0083
              M ~ V - 1 ;                                               SFSB0084
                                                                        SFSB0085
HERE :        TT[M] ~ POLY[DEGREE[M]] | TT[M+1] + TT[M] ;               SFSB0086
              TT[M+1] ~ 0 ;                                             SFSB0087
              DEGREE[M] ~ DEGREE[M] + 1 ;                               SFSB0088
              IF DEGREE[M] { TADD[M] + P[M] THEN GO TO BACK ;           SFSB0089
              IF M = 1 THEN GO TO FINI ;                                SFSB0090
              DEGREE[M] ~ TADD[M] ;          M ~ M - 1 ;                SFSB0091
              GO TO HERE ;                                              SFSB0092
                                                                        SFSB0093
FINI :        IF B THEN                                                 SFSB0094
         BEGIN                                                          SFSB0095
              SUM[KV] ~ TT[1] ;          GO TO D2                       SFSB0096
         END ;                                                          SFSB0097
              PHI ~ TT[1]                                               SFSB0098
         END DEPENDENT ;                                                SFSB0099
                                                                        SFSB0100
              FOR KV ~ 1 STEP 1 UNTIL V DO                              SFSB0101
         BEGIN                                                          SFSB0102
              M ~ KV ;                                                  SFSB0103
              IF S[M] THEN                                              SFSB0104
         BEGIN                                                          SFSB0105
              DATUM ~ DATA[M] ;                                         SFSB0106
                                                                        SFSB0107
COMMENT       EVALUATE THE PARTIAL POLYNOMIALS ;                        SFSB0108
                                                                        SFSB0109
PARTCOMPUTE :                                                           SFSB0110
         BEGIN                                                          SFSB0111
              POLY[T] ~ 0 ;          POLY[T+1] ~ 1.0 ;                  SFSB0112
              T ~ T + 2 ;                                               SFSB0113
                                                                        SFSB0114
              FOR I ~ 2 STEP 1 UNTIL P[M] DO                            SFSB0115
         BEGIN                                                          SFSB0116
              POLY[T] ~ (ALEPH[M,I] + DATUM) | POLY[T-1] +              SFSB0117
              POLY[ADD[M]+I-1] + BETA[M,I] | POLY[T-2] ;                SFSB0118
              T ~ T + 1                                                 SFSB0119
         END                                                            SFSB0120
         END PARTCOMPUTE ;                                              SFSB0121
              T ~ TADD[M] ~ ADD[V+1] ;          B ~ TRUE ;              SFSB0122
              GO TO DEPENDENT ;                                         SFSB0123
                                                                        SFSB0124
D2 :          TADD[KV] ~ ADD[KV]                                        SFSB0125
         END                                                            SFSB0126
         END                                                            SFSB0127
         END PHIPART ;                                                  SFSB0128
