PROCEDURE     KPOLY (T, N, CALCPT, K, PKT) ;                            KPYP0001

COMMENT       RELATIVE VOLATILITY K-RATIO PROCEDURE.                    KPYP0002
              BY DONALD L. VOSS                                         KPYP0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KPYP0004
              CARD SEQUENCE CODE STARTS WITH KPYP0001                   KPYP0005
              FIRST RELEASE DATE:  5-5-63                               KPYP0006

              THIS PROCEDURE CALCULATES VAPOR-LIQUID EQUILIBRIUM        KPYP0007
              K-RATIOS (Y/X) AND THEIR DERIVATIVES WITH RESPECT TO      KPYP0008
              TEMPERATURE BY MEANS OF A RELATIVE VOLATILITY EQUATION.   KPYP0009
              THE REFERENCE COMPONENT K-RATIO IS CALCULATED BY MEANS    KPYP0010
              OF A FOURTH DEGREE POLYNOMIAL IN TEMPERATURE.  A SECOND   KPYP0011
              DEGREE POLYNOMIAL IS USED FOR THE RELATIVE VOLATILITIES   KPYP0012
              OF THE OTHER COMPONENTS.                                  KPYP0013

              THE INPUT PARAMETERS ARE                                  KPYP0014
              T    - TEMPERATURE, F.  - REAL                            KPYP0015
              N    - NUMBER OF COMPONENTS  - INTEGER                    KPYP0016
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKT.                KPYP0017

              IN ADDITION, THE VARIABLES KC4 AND KC5 ARE NON-LOCAL TO   KPYP0018
              KPOLY.                                                    KPYP0019
              KC4  - REFERENCE COMPONENT K-RATIO COEFFICIENTS  - REAL   KPYP0020
                   ARRAY [0:5]                                          KPYP0021
              KC5  - RELATIVE VOLATILITY COEFFICIENTS OF COMPONENTS  -  KPYP0022
                   REAL ARRAY [0:N,0:3]                                 KPYP0023

              THE OUTPUT PARAMETERS ARE                                 KPYP0024
              K    - COMPONENT K-RATIO  - REAL ARRAY [0:N]              KPYP0025
              PKT  - T DERIVATIVE OF K  - REAL ARRAY [0:N]  ;           KPYP0026

VALUE         T, N, CALCPT ;                                            KPYP0027
INTEGER       N ;                                                       KPYP0028
BOOLEAN       CALCPT ;                                                  KPYP0029
REAL          T ;                                                       KPYP0030
ARRAY         K[0], PKT[0] ;                                            KPYP0031

         BEGIN                                                          KPYP0032
INTEGER       I ;                                                       KPYP0033
REAL          TRS1, KR, PKRT1, RV ;                                     KPYP0034

COMMENT       CALCULATE THE K-RATIO OF THE REFERENCE COMPONENT, KR.  ;  KPYP0035
              TRS1 ~ 0.01 | T ;                                         KPYP0036
              KR ~ (((KC4[5] | TRS1 + KC4[4]) | TRS1 + KC4[3]) | TRS1 + KPYP0037
              KC4[2]) | TRS1 + KC4[1] ;                                 KPYP0038

COMMENT       CALCULATE THE TEMPERATURE DERIVATIVE OF KR, MULTIPLIED BY KPYP0039
              100.0, IF CALCPT IS TRUE.  ;                              KPYP0040
              IF CALCPT THEN                                            KPYP0041
              PKRT1 ~ ((KC4[5] | TRS1 | 4.0 + KC4[4] | 3.0) | TRS1 +    KPYP0042
              KC4[3] | 2.0) | TRS1 + KC4[2] ;                           KPYP0043

COMMENT       CALCULATE THE COMPONENT K-RATIOS AND, IF CALCPT IS TRUE,  KPYP0044
              THEIR TEMPERATURE DERIVATIVES.  ;                         KPYP0045
              FOR I ~ 1 STEP 1 UNTIL N DO                               KPYP0046
         BEGIN                                                          KPYP0047
              RV ~ (KC5[I,3] | TRS1 + KC5[I,2]) | TRS1 + KC5[I,1] ;     KPYP0048
              K[I] ~ KR | RV ;                                          KPYP0049
              IF CALCPT THEN                                            KPYP0050
              PKT[I] ~ ((KC5[I,3] | TRS1 | 2.0 + KC5[I,2]) | KR +       KPYP0051
              RV | PKRT1) | 0.01                                        KPYP0052
         END                                                            KPYP0053

         END  KPOLY ;                                                   KPYP0054
