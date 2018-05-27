                                                                        HLPP0002
COMMENT       THIS PROCEDURE TIES TOGETHER THE VARIOUS CORRELATIONS FOR HLPP0003
              CALCULATING LIQUID ENTHALPY.  THE HLIQ PROCEDURE, THE     HLPP0004
              KELLOGG CORRELATION, AND AN INTERPOLATION METHOD ARE      HLPP0005
              INCLUDED.                                                 HLPP0006
              BY DONALD L. VOSS                                         HLPP0007
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       HLPP0008
              CARD SEQUENCE CODE STARTS WITH HLPP0001.                  HLPP0009
              FIRST RELEASE DATE       1-1-64                           HLPP0010
                                                                        HLPP0011
              THE INPUT PARAMETERS ARE                                  HLPP0012
              N    - TOTAL NUMBER OF COMPONENTS  - INTEGER              HLPP0013
              T    - TEMPERATURE, F.  - REAL                            HLPP0014
              X    - COMPONENT MOL FRACTIONS - REAL ARRAY [0:N]         HLPP0015
              MW   - COMPONENT MOLECULAR WEIGHTS (NECESSARY ONLY FOR    HLPP0016
                   KELLOGG CORRELATION)  - REAL ARRAY [0:N]             HLPP0017
              PICKHL - CHOICE OF HL CORRELATIONS - INTEGER              HLPP0018
                   = 1 FOR HLIQ                                         HLPP0019
                   = 2 FOR KELLOGG CORRELATION                          HLPP0020
                   = 3 FOR INTERPOLATION METHOD                         HLPP0021
              CALCDHL - BOOLEAN ARRAY [0:2]                             HLPP0022
                   IF CALCDHL[1] IS TRUE, CALCULATE PHLT.               HLPP0023
                   IF CALCDHL[2] IS TRUE, CALCULATE PHLX.               HLPP0024
              IN ADDITION, THE VARIABLES HLC1 TO HLC5, TR1S3, TR1S4,    HLPP0025
                   AND TB1S1 ARE NON-LOCAL TO THE HLPICK PROCEDURE.     HLPP0026
              HLC1 TO HLC4 - REFER TO THE KELLOGG CORRELATION (PTS-47)  HLPP0027
              HLC5 - REFER TO THE HLIQ PROCEDURE (PTS-28)               HLPP0028
              HLC6 AND HLC7 - REFER TO THE INTERPOLATION METHOD (PTS-47)HLPP0029
              TR1S3[0:2], TR1S4[0:2], AND TB1S1[0:2] ARE TEMPORARY      HLPP0030
                   STORAGE ARRAYS.  R = REAL, B = BOOLEAN.              HLPP0031
                                                                        HLPP0032
              THE OUTPUT PARAMETERS ARE                                 HLPP0033
              HL   - LIQUID ENTHALPY OF MIXTURE, BTU/MOL - REAL         HLPP0034
              PHLT - PARTIAL DERIVATIVE OF HL WITH RESPECT TO T ,       HLPP0035
                   BTU/MOL - DEGREE F - REAL                            HLPP0036
              PHLX - PARTIAL DERIVATIVE OF HL WITH RESPECT TO MOL       HLPP0037
                   FRACTION, BTU/MOL - REAL ARRAY [0:N]                 HLPP0038
                                                                        HLPP0039
              THE PROCEDURES HLIQ AND PHIPART MUST BE DECLARED PRIOR TO HLPP0040
                   HLPICK. ;                                            HLPP0041
                                                                        HLPP0042
VALUE         N, T ;                                                    HLPP0043
REAL          T, HL, PHLT ;                                             HLPP0044
INTEGER       N, PICKHL ;                                               HLPP0045
REAL ARRAY    X[0], MW[0], PHLX[0] ;                                    HLPP0046
BOOLEAN ARRAY CALCDHL[0] ;                                              HLPP0047
                                                                        HLPP0048
         BEGIN                                                          HLPP0049
REAL          XMW, SUMX, PDRV, H1, H2, T1, T2, SLOPE, PHLXI ;           HLPP0050
INTEGER       I, J, M ;                                                 HLPP0051
LABEL         HL1 ;                                                     HLPP0052
                                                                        HLPP0053
COMMENT       ENTER HLIQ IF SELECTED ;                                  HLPP0054
              IF PICKHL = 1   THEN                                      HLPP0055
              HLIQ (N, T, X, HL, PHLX, PHLT) ;                          HLPP0056
                                                                        HLPP0057
COMMENT       ENTER PHIPART FOR KELLOGG CORRELATION IF SELECTED ;       HLPP0058
              IF PICKHL = 2   THEN                                      HLPP0059
         BEGIN                                                          HLPP0060
              XMW ~ SUMX ~ 0 ;                                          HLPP0061
              FOR I ~ 1 STEP 1 UNTIL N   DO                             HLPP0062
         BEGIN                                                          HLPP0063
              XMW ~ X[I] | MW[I] + XMW ;                                HLPP0064
              SUMX ~ X[I] + SUMX                                        HLPP0065
         END ;                                                          HLPP0066
              TR1S3[1] ~ XMW / SUMX ;     TR1S3[2] ~ T ;                HLPP0067
              TB1S1[1] ~ CALCDHL[2] ;     TB1S1[2] ~ CALCDHL[1] ;       HLPP0068
              PHIPART (2, TR1S3, HLC1, HLC2, HLC3, HLC4, TB1S1, HL,     HLPP0069
              TR1S4) ;                                                  HLPP0070
              HL ~ HL | SUMX ;     PHLT ~ TR1S4[2] | SUMX ;             HLPP0071
              IF CALCDHL[2]   THEN                                      HLPP0072
         BEGIN                                                          HLPP0073
              PDRV ~ TR1S4[1] ;                                         HLPP0074
              FOR I ~ 1 STEP 1 UNTIL N  DO                              HLPP0075
              PHLX[I] ~ MW[I] | PDRV                                    HLPP0076
         END                                                            HLPP0077
         END ;                                                          HLPP0078
                                                                        HLPP0079
COMMENT       USE THE LINEAR INTERPOLATION METHOD IF SELECTED ;         HLPP0080
              IF PICKHL = 3   THEN                                      HLPP0081
         BEGIN                                                          HLPP0082
              HL ~ PHLT ~ 0 ;                                           HLPP0083
              FOR I ~ 1 STEP 1 UNTIL N   DO                             HLPP0084
         BEGIN                                                          HLPP0085
              M ~ HLC6[I,0] ;                                           HLPP0086
              FOR J ~ 2 STEP 1 UNTIL M   DO                             HLPP0087
              IF T < HLC6[I,J]   THEN   GO TO HL1 ;                     HLPP0088
              J ~ M ;                                                   HLPP0089
HL1:          H1 ~ HLC7[I,J-1] ;      H2 ~ HLC7[I,J] ;                  HLPP0090
              T1 ~ HLC6[I,J-1] ;      T2 ~ HLC6[I,J] ;                  HLPP0091
              SLOPE ~ (H2 - H1) / (T2 - T1) ;                           HLPP0092
              PHLXI ~ H1 + SLOPE | (T - T1) ;                           HLPP0093
              HL ~ X[I] | PHLXI + HL ;                                  HLPP0094
              PHLT ~ X[I] | SLOPE + PHLT ;                              HLPP0095
              PHLX[I] ~ PHLXI                                           HLPP0096
         END                                                            HLPP0097
         END                                                            HLPP0098
                                                                        HLPP0099
         END  HLPICK ;                                                  HLPP0100
