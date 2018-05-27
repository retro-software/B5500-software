PROCEDURE     HVPICK (N, T, P, Y, TC, PC, MW, PICKHV, CALCDHV, HV,      HVPP0001
              PHVT, PHVP, PHVY) ;                                       HVPP0002
                                                                        HVPP0003
COMMENT       THIS PROCEDURE TIES TOGETHER THE VARIOUS CORRELATIONS FOR HVPP0004
              CALCULATING VAPOR ENTHALPY.  THE HVPCOR PROCEDURE, THE    HVPP0005
              KELLOGG CORRELATION, AND AN INTERPOLATION METHOD ARE      HVPP0006
              INCLUDED.                                                 HVPP0007
              BY DONALD L. VOSS                                         HVPP0008
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION        HVPP0009
              CARD SEQUENCE CODE STARTS WITH HVPP0001.                  HVPP0010
              FIRST RELEASE DATE       1-1-64                           HVPP0011
                                                                        HVPP0012
              THE INPUT PARAMETERS ARE                                  HVPP0013
              N    - TOTAL NUMBER OF COMPONENTS  - INTEGER              HVPP0014
              T    - TEMPERATURE, F  - REAL                             HVPP0015
              P    - PRESSURE, PSIA  - REAL                             HVPP0016
              Y    - COMPONENT MOL FRACTIONS - REAL ARRAY [0:N]         HVPP0017
              TC   - COMPONENT CRITICAL TEMPERATURE, R  - REAL          HVPP0018
                   ARRAY [0:N].  NECESSARY ONLY FOR HVPCOR.             HVPP0019
              PC   - COMPONENT CRITICAL PRESSURE, PSIA  - REAL ARRAY    HVPP0020
                   [0:N].  NECESSARY ONLY FOR HVPCOR.                   HVPP0021
              MW   - COMPONENT MOLECULAR WEIGHT  - REAL ARRAY [0:N].    HVPP0022
                   NECESSARY ONLY FOR KELLOGG CORRELATION.              HVPP0023
              PICKHV - CHOICE OF HV CORRELATIONS - INTEGER              HVPP0024
                   = 1 FOR HVPCOR                                       HVPP0025
                   = 2 FOR KELLOGG CORRELATION                          HVPP0026
                   = 3 FOR INTERPOLATION METHOD                         HVPP0027
              CALCDHV - BOOLEAN ARRAY [0:3]                             HVPP0028
                   IF CALCDHV[1] IS TRUE, CALCULATE PHVT.               HVPP0029
                   IF CALCDHV[2] IS TRUE, CALCULATE PHVP.               HVPP0030
                   IF CALCDHV[3] IS TRUE, CALCULATE PHVY.               HVPP0031
              IN ADDITION, THE VARIALBLES HVC1-5 AND HVC7-15, TR1S3,    HVPP0032
                   TR1S4, AND TB1S1 ARE NON-LOCAL TO HVPICK.            HVPP0033
              HVC1-5 AND HVC7-9 - REFER TO THE HVPCOR PROCEDURE (PTS-31)HVPP0034
              HVC10-13 - REFER TO THE KELLOGG CORRELATION (PTS-49)      HVPP0035
              HVC14-15 - REFER TO THE INTERPOLATION METHOD (PTS-49)     HVPP0036
              TR1S3[0:3], TR1S4[0:3], AND TB1S1[0:3] ARE TEMPORARY      HVPP0037
                   STORAGE ARRAYS.  R = REAL, B = BOOLEAN.              HVPP0038
                                                                        HVPP0039
              THE OUTPUT PARAMETERS ARE                                 HVPP0040
              HV   - ENTHALPY OF VAPOR MIXTURE, BTU/MOL - REAL          HVPP0041
              PHVT - PARTIAL DERIVATIVE OF HV WITH RESPECT TO T,        HVPP0042
                   BTU/MOL - DEGREE F - REAL                            HVPP0043
              PHVP - PARTIAL DERIVATIVE OF HV WITH RESPECT TO P,        HVPP0044
                   BTU/MOL - PSIA - REAL                                HVPP0045
              PHVY - PARTIAL DERIVATIVE OF HV WITH RESPECT TO MOL       HVPP0046
                   FRACTION, BTU/MOL - REAL ARRAY [0:N]                 HVPP0047
                                                                        HVPP0048
              THE PROCEDURES HVPCOR AND PHIPART MUST BE DECLARED PRIOR  HVPP0049
                   TO HVPICK. ;                                         HVPP0050
                                                                        HVPP0051
VALUE         N, T, P, TC, PC ;                                         HVPP0052
REAL          T, P, HV, PHVT, PHVP ;                                    HVPP0053
INTEGER       N, PICKHV ;                                               HVPP0054
REAL ARRAY    Y[0], TC[0], PC[0], MW[0], PHVY[0] ;                      HVPP0055
BOOLEAN ARRAY CALCDHV[0]  ;                                             HVPP0056
                                                                        HVPP0057
         BEGIN                                                          HVPP0058
REAL          YMW, SUMY, PDRV, H1, H2, T1, T2, SLOPE, PHVYI ;           HVPP0059
INTEGER       I, J, M ;                                                 HVPP0060
LABEL         HV1 ;                                                     HVPP0061
                                                                        HVPP0062
COMMENT       ENTER HVPCOR PROCEDURE IF SELECTED ;                      HVPP0063
              IF PICKHV = 1   THEN                                      HVPP0064
              HVPCOR (N, T, P, Y, CALCDHV, HV, PHVT, PHVP, PHVY) ;      HVPP0065
                                                                        HVPP0066
COMMENT       ENTER PHIPART PROCEDURE FOR KELLOGG CORRELATION IF        HVPP0067
              SELECTED ;                                                HVPP0068
              IF PICKHV = 2   THEN                                      HVPP0069
         BEGIN                                                          HVPP0070
              YMW ~ SUMY ~ 0 ;                                          HVPP0071
              FOR I ~ 1 STEP 1 UNTIL N   DO                             HVPP0072
         BEGIN                                                          HVPP0073
              YMW ~ Y[I] | MW[I] + YMW ;                                HVPP0074
              SUMY ~ Y[I] + SUMY                                        HVPP0075
         END ;                                                          HVPP0076
              TR1S3[1] ~ P ;   TR1S3[2] ~ YMW/SUMY ;   TR1S3[3] ~ T ;   HVPP0077
              TB1S1[1] ~ CALCDHV[2] ;     TB1S1[2] ~ CALCDHV[3] ;       HVPP0078
              TB1S1[3] ~ CALCDHV[1] ;                                   HVPP0079
              PHIPART (3, TR1S3, HVC10, HVC11, HVC12, HVC13, TB1S1, HV, HVPP0080
              TR1S4) ;                                                  HVPP0081
              HV ~ HV | SUMY ;                                          HVPP0082
              PHVT ~ TR1S4[3] | SUMY ;     PHVP ~ TR1S4[1] | SUMY ;     HVPP0083
              IF CALCDHV[3]   THEN                                      HVPP0084
         BEGIN                                                          HVPP0085
              PDRV ~ TR1S4[2] ;                                         HVPP0086
              FOR I ~ 1 STEP 1 UNTIL N   DO                             HVPP0087
              PHVY[I] ~ MW[I] | PDRV                                    HVPP0088
         END                                                            HVPP0089
         END ;                                                          HVPP0090
                                                                        HVPP0091
COMMENT       USE THE LINEAR INTERPOLATION METHOD IF SELECTED ;         HVPP0092
              IF PICKHV = 3   THEN                                      HVPP0093
         BEGIN                                                          HVPP0094
              HV ~ PHVT ~ 0 ;                                           HVPP0095
              FOR I ~ 1 STEP 1 UNTIL N   DO                             HVPP0096
         BEGIN                                                          HVPP0097
              M ~ HVC14[I,0] ;                                          HVPP0098
              FOR J ~ 2 STEP 1 UNTIL M   DO                             HVPP0099
              IF T < HVC14[I,J]   THEN   GO TO HV1 ;                    HVPP0100
              J ~ M ;                                                   HVPP0101
HV1:          H1 ~ HVC15[I,J-1] ;      H2 ~ HVC15[I,J] ;                HVPP0102
              T1 ~ HVC14[I,J-1] ;      T2 ~ HVC14[I,J] ;                HVPP0103
              SLOPE ~ (H2 - H1) / (T2 - T1) ;                           HVPP0104
              PHVYI ~ H1 + SLOPE | (T - T1) ;                           HVPP0105
              HV ~ Y[I] | PHVYI + HV ;                                  HVPP0106
              PHVT ~ Y[I] | SLOPE + PHVT ;                              HVPP0107
              PHVY[I] ~ PHVYI                                           HVPP0108
         END                                                            HVPP0109
         END                                                            HVPP0110
                                                                        HVPP0111
         END  HVPICK ;                                                  HVPP0112
