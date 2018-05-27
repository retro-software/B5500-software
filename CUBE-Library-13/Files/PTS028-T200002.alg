PROCEDURE     HLIQ ( N, T, Z, HL, PHLX, PHLT ) ;                        HLIQ   1

COMMENT       THIS PROCEDURE COMPUTES THE TOTAL ENTHALPY, PARTIAL       HLIQ   2
              MOLAL ENTHALPIES, AND THE HEAT CAPACITY OF A LIQUID       HLIQ   3
              MIXTURE, USING EXTENDED LIQUID LINES.                     HLIQ   4
              A. PAUL OLESON                                            HLIQ   5
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       HLIQ   6
              CARD SEQUENCE STARTS WITH HLIQ-0001.                      HLIQ   7
              FIRST RELEASE 3-20-63.                                    HLIQ   8

              THE INPUT PARAMETERS ARE                                  HLIQ   9
              N    - NUMBER OF COMPONENTS - INTEGER                     HLIQ  10
              T    - TEMPERATURE, F - REAL                              HLIQ  11
              Z    - MOL FRACTIONS - REAL ARRAY [0:N]                   HLIQ  12

              THE OUTPUT PARAMETERS ARE                                 HLIQ  13
              HL   - LIQUID ENTHALPY, BTU/MOL - REAL                    HLIQ  14
              PHLX - PARTIAL MOLAL ENTHALPY, BTU/MOL - REAL ARRAY [0:N] HLIQ  15
              PHLT - LIQUID HEAT CAPACITY, BTU/MOL/DEGREE F - REAL ;    HLIQ  16

VALUE         N, T ;                                                    HLIQ  17

INTEGER       N ;                                                       HLIQ  18

REAL          T, HL, PHLT ;                                             HLIQ  19

ARRAY         Z[0], PHLX[0] ;                                           HLIQ  20

         BEGIN                                                          HLIQ  21

INTEGER       I ;                                                       HLIQ  22

REAL          ZI, T2, B, CT, DT2 ;                                      HLIQ  23

              HL ~ PHLT ~ 0.0 ;              T2 ~ T | T ;               HLIQ  24
              FOR I ~ 1 STEP 1 UNTIL N DO                               HLIQ  25
         BEGIN                                                          HLIQ  26
              ZI ~ Z[I] ;                    B ~ HLC5[I,2] ;            HLIQ  27
              CT ~ HLC5[I,3] | T ;           DT2 ~ HLC5[I,4] | T2 ;     HLIQ  28
              PHLX[I] ~ HLC5[I,1] + ( B + CT + DT2 ) | T ;              HLIQ  29
              HL ~ PHLX[I] | ZI + HL ;                                  HLIQ  30
              PHLT ~ ( DT2 | 3.0 + CT | 2.0 + B ) | ZI + PHLT           HLIQ  31
         END                                                            HLIQ  32
         END HLIQ ;                                                     HLIQ  33
