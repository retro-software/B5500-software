PROCEDURE     KWATER (T, P, CALCPT, CALCPP, KW, PVW, XW, PKWT, PKWP,    KWRP0001
              PPVWT, PXWT) ;                                            KWRP0002

COMMENT       PROCEDURE FOR K-RATIO OF WATER DISSOLVED IN HYDROCARBONS. KWRP0003
              BY A. PAUL OLESON AND DONALD L. VOSS                      KWRP0004
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KWRP0005
              FIRST RELEASE DATE:  5-5-63                               KWRP0006

              THIS PROCEDURE CALCULATES THE FOLLOWING QUANTITIES FOR    KWRP0007
              WATER: VAPOR-LIQUID EQUILIBRIUM RATIO (KW OR Y/X), VAPOR  KWRP0008
              PRESSURE, MOLE FRACTION OF WATER DISSOLVED IN THE HYDRO-  KWRP0009
              CARBON PHASE, THE DERIVATIVES OF THESE QUANTITIES WITH    KWRP0010
              RESPECT TO TEMPERATURE, AND THE DERIVATIVE OF KW WITH     KWRP0011
              RESPECT TO PRESSURE.  THE K-RATIO OF WATER IS CALCULATED  KWRP0012
              BY ASSUMING IDEAL VAPOR BEHAVIOR AND USING AN EMPIRICAL   KWRP0013
              CORRELATION FOR THE WATER SOLUBILITY.                     KWRP0014

              THE INPUT PARAMETERS ARE                                  KWRP0015
              T    - TEMPERATURE, F. - REAL                             KWRP0016
              P    - PRESSURE, PSIA - REAL                              KWRP0017
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKWT, PPVWT, AND    KWRP0018
                     PXWT.                                              KWRP0019
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE PKWP.               KWRP0020

              ALSO, THE VARIABLES KC3 AND KC6 ARE NON-LOCAL.            KWRP0021
              KC3  - WATER VAPOR PRESSURE COEFFIENTS - REAL ARRAY [0:3].KWRP0022
                     KC3 IS USED AS ANTC IN THE ANT1 PROCEDURE.         KWRP0023
              KC6  - COEFFICIENTS FOR THE SOLUBILITY OF WATER IN HYDRO- KWRP0024
                     CARBONS - REAL ARRAY [0:3].  KC6 IS USED AS ANTC   KWRP0025
                     IN THE ANT1 PROCEDURE.                             KWRP0026

              THE OUTPUT PARAMETERS ARE                                 KWRP0027
              KW   - VAPOR-LIQUID EQUILIBRIUM RATIO (Y/X) OF WATER -    KWRP0028
                     REAL                                               KWRP0029
              PVW  - VAPOR PRESSURE OF WATER, PSIA                      KWRP0030
              XW   - MOLE FRACTION OF WATER DISSOLVED IN HYDROCARBON    KWRP0031
                     PHASE - REAL                                       KWRP0032
              PKWT - T DERIVATIVE OF KW - REAL                          KWRP0033
              PKWP - P DERIVATIVE OF KW - REAL                          KWRP0034
              PPVWT - T DERIVATIVE OF PVW - REAL                        KWRP0035
              PXWT - T DERIVATIVE OF XW - REAL                          KWRP0036

              THE ANT1 PROCEDURE MUST BE DECLARED PRIOR TO KWATER ;     KWRP0037

VALUE         T, P, CALCPT, CALCPP ;                                    KWRP0038
BOOLEAN       CALCPT, CALCPP ;                                          KWRP0039
REAL          T, P, KW, PVW, XW, PKWT, PKWP, PPVWT, PXWT ;              KWRP0040

         BEGIN                                                          KWRP0041

COMMENT       ENTER THE ANT1 PROCEDURE TO CALCULATE THE VAPOR PRESSURE  KWRP0042
              OF WATER, AND ITS TEMPERATURE DERIVATIVE. ;               KWRP0043
              ANT1 (T, CALCPT, KC3, PVW, PPVWT) ;                       KWRP0044

COMMENT       ENTER THE ANT1 PROCEDURE TO CALCULATE THE SOLUBILITY OF   KWRP0045
              WATER IN HYDROCARBONS, AND ITS TEMPERATURE DERIVATIVE. ;  KWRP0046
              ANT1 (T, CALCPT, KC6, XW, PXWT) ;                         KWRP0047

COMMENT       CALCULATE THE K-RATIO OF WATER. ;                         KWRP0048
              KW ~ PVW/(P | XW) ;                                       KWRP0049

COMMENT       IF CALCPT IS TRUE, CALCULATE THE T DERIVATIVE OF KW. ;    KWRP0050
              IF CALCPT THEN                                            KWRP0051
              PKWT ~ (-PXWT/XW + PPVWT/PVW) | KW ;                      KWRP0052

COMMENT       IF CALCPP IS TRUE, CALCULATE THE P DERIVATIVE OF KW. ;    KWRP0053
              IF CALCPP THEN                                            KWRP0054
              PKWP ~ -KW/P                                              KWRP0055

         END  KWATER ;                                                  KWRP0056
