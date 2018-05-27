PROCEDURE     ANT1 (X, CALCPX, ANTC, ANT, PANTX) ;                      ANTP0001

COMMENT       ANTOINE EQUATION PROCEDURE.                               ANTP0002
              BY DONALD L. VOSS                                         ANTP0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       ANTP0004
              FIRST RELEASE DATE:  5-5-63                               ANTP0005

              THIS PROCEDURE EVALUATES THE 3-CONSTANT ANTOINE EQUATION. ANTP0006

              THE INPUT PARAMETERS ARE                                  ANTP0007
              X    - INDEPENDENT VARIABLE - REAL                        ANTP0008
              CALCPX - BOOLEAN.  IF TRUE, CALCULATE PANTX.              ANTP0009
              ANTC - ANTOINE EQUATION COEFFICIENTS - REAL ARRAY [0:3]   ANTP0010

              THE OUTPUT PARAMETERS ARE                                 ANTP0011
              ANT  - VALUE OF THE ANTOINE EQUATION AT X - REAL          ANTP0012
              PANTX - X DERIVATIVE OF ANT - REAL ;                      ANTP0013

VALUE         X, CALCPX ;                                               ANTP0014
BOOLEAN       CALCPX ;                                                  ANTP0015
REAL          X, ANT, PANTX ;                                           ANTP0016
ARRAY         ANTC[0] ;                                                 ANTP0017

         BEGIN                                                          ANTP0018
REAL          TRS1 ;                                                    ANTP0019

COMMENT       EVALUATE THE ANTOINE EQUATION ;                           ANTP0020
              TRS1 ~ ANTC[3] + X ;                                      ANTP0021
              ANT ~ EXP (-ANTC[2]/TRS1 + ANTC[1] ) ;                    ANTP0022

COMMENT       IF CALCPX IS TRUE, CALCULATE THE DERIVATIVE OF THE        ANTP0023
              ANTOINE EQUATION WITH RESPECT TO X. ;                     ANTP0024
              IF CALCPX THEN                                            ANTP0025
              PANTX ~ ANTC[2]/TRS1*2 | ANT                              ANTP0026

         END  ANT1 ;                                                    ANTP0027
