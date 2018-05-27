PROCEDURE     KIDEAL (T, P, N, CALCPT, CALCPP, K, PKT, PKP) ;           KIDP0001

COMMENT       IDEAL K-RATIO PROCEDURE.                                  KIDP0002
              BY DONALD L. VOSS                                         KIDP0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KIDP0004
              FIRST RELEASE DATE:  5-5-63                               KIDP0005

              THIS PROCEDURE CALCULATES VAPOR-LIQUID EQUILIBRIUM RATIOS KIDP0006
              (Y/X), AND THEIR DERIVATIVES WITH RESPECT TO TEMPERATURE  KIDP0007
              AND PRESSURE.  IDEAL LIQUID AND VAPOR BEHAVIOR IS ASSUMED.KIDP0008

              THE INPUT PARAMETERS ARE                                  KIDP0009
              T    - TEMPERATURE, F - REAL                              KIDP0010
              P    - PRESSURE, PSIA - REAL                              KIDP0011
              N    - NUMBER OF COMPONENTS - INTEGER                     KIDP0012
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKT.                KIDP0013
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE PKP.                KIDP0014

              ALSO, THE VARIABLE KC2 IS NON-LOCAL.                      KIDP0015
              KC2  - COMPONENT VAPOR PRESSURE COEFFICIENTS - REAL ARRAY KIDP0016
                     [0:N,0:3].  KC2 IS USED AS ANTC IN THE ANT1        KIDP0017
                     PROCEDURE.                                         KIDP0018

              THE OUTPUT PARAMETERS ARE                                 KIDP0019
              K    - COMPONENT VAPOR-LIQUID EQUILIBRIUM RATIOS (Y/X) -  KIDP0020
                     REAL ARRAY [0:N]                                   KIDP0021
              PKT  - COMPONENT T DERIVATIVES OF K - REAL ARRAY [0:N]    KIDP0022
              PKP  - COMPONENT P DERIVATIVES OF K - REAL ARRAY [0:N]    KIDP0023

              THE PROCEDURE ANT1 MUST BE DECLARED PRIOR TO KIDEAL. ;    KIDP0024

VALUE         T, P, N, CALCPT, CALCPP ;                                 KIDP0025
INTEGER       N ;                                                       KIDP0026
BOOLEAN       CALCPT, CALCPP ;                                          KIDP0027
REAL          T, P ;                                                    KIDP0028
ARRAY         K[0], PKT[0], PKP[0] ;                                    KIDP0029

         BEGIN                                                          KIDP0030
INTEGER       I ;                                                       KIDP0031
REAL          TRS1 ;                                                    KIDP0032

              TRS1 ~ 1.0/P ;                                            KIDP0033
              FOR  I ~ 1 STEP 1 UNTIL N DO                              KIDP0034
         BEGIN                                                          KIDP0035

COMMENT       ENTER THE ANT1 PROCEDURE TO CALCULATE THE VAPOR PRESSURE  KIDP0036
              AND ITS TEMPERATURE DERIVATIVE FOR EACH COMPONENT. ;      KIDP0037
              ANT1 (T, CALCPT, KC2[I,*], K[I], PKT[I] ) ;               KIDP0038

COMMENT       CALCULATE THE COMPONENT K-RATIOS. ;                       KIDP0039
              K[I] ~ K[I] | TRS1 ;                                      KIDP0040

COMMENT       IF CALCPT IS TRUE, CALCULATE THE COMPONENT T DERIVATIVES  KIDP0041
              OF THE K-RATIOS. ;                                        KIDP0042
              IF CALCPT THEN                                            KIDP0043
              PKT[I] ~ PKT[I] | TRS1 ;                                  KIDP0044

COMMENT       IF CALCPP IS TRUE, CALCULATE THE COMPONENT P DERIVATIVES  KIDP0045
              OF THE K-RATIOS. ;                                        KIDP0046
              IF CALCPP THEN                                            KIDP0047
              PKP[I] ~ -K[I] | TRS1                                     KIDP0048
         END                                                            KIDP0049

         END  KIDEAL ;                                                  KIDP0050
