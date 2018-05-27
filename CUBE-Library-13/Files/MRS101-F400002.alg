COMMENT       THIS PROCEDURE WILL COMPUTE A SINGLE ROW OF THE INVERSE   MONT0001
              OF A GIVEN MATRIX USING A MONTE-CARLO TECHNIQUE.  THE     MONT0002
              CRITERIA FOR CONVERGENCE AND OTHER PERTINENT INFORMATION  MONT0003
              ARE DISCUSSED IN THE  CORRESPONDING TECHNICAL BULLETIN.   MONT0004
              R.D. RODMAN,                                              MONT0005
              (PROFESSIONAL SERVICES GROUP).                            MONT0006
              CARD SEQUENCE STARTS WITH MONT0000.                       MONT0007
              FIRST RELEASE 1/1/62.  ;                                  MONT0008
PROCEDURE     MONTECARLO(N, A, ROW, TOL, MXM, INV, TEST, COUNT) ;       MONT0009
VALUE         N, ROW, TOL, MXM ;                                        MONT0010
INTEGER       N, ROW, MXM, COUNT ;                                      MONT0011
REAL          TOL ;                                                     MONT0012
REAL ARRAY    A[0,0], INV, TEST[0] ;                                    MONT0013
         BEGIN                                                          MONT0014
INTEGER       I, J, K, NWK, LASTWALK, WALK, RN ;                        MONT0015
REAL          RES, P, G, D ;                                            MONT0016
REAL ARRAY    SUM[0:N],V[0:N,0:N] ;                                     MONT0017
LABEL         START1, START2, STOP, EXIT ;                              MONT0018
PROCEDURE     RANDOM(X) ;                                               MONT0019
INTEGER       X ;                                                       MONT0020
         BEGIN                                                          MONT0021
INTEGER       T ;                                                       MONT0022
              T ~ 0 ;                                                   MONT0023
              T.[12:19] ~ X.[29:19] ;                                   MONT0024
              X.[12:36] ~ X.[12:36] + T.[12:36] + 127                   MONT0025
         END ;                                                          MONT0026
COMMENT       PROCEDURE BEGINS EXECUTION HERE ;                         MONT0027
              RN ~ 30517578125 ;                                        MONT0028
COMMENT       "P" IS THE WALK PROBABILITY FOR EACH ELEMENT PAIR.  THE   MONT0029
              "STOP" PROBABILITY WILL ALWAYS BE 1/N ;                   MONT0030
              P ~ (N-1) / (N|N) ;   D ~ 1/(68719476736 | P) ;           MONT0031
COMMENT       THE MATRIX OF "TRANSITION VALUES" IS COMPUTED ;           MONT0032
              FOR I ~ 1 STEP 1 UNTIL N DO                               MONT0033
              FOR J ~ 1 STEP 1 UNTIL N DO                               MONT0034
              V[I,J] ~ IF I =J THEN (1-A[I,J])/P ELSE -A[I,J]/P ;       MONT0035
              NWK ~ 1000 ;                                              MONT0036
              COUNT ~ RES ~ 0 ;                                         MONT0037
              FOR K ~ 1 STEP 1 UNTIL N DO                               MONT0038
              TEST[K] ~ SUM[K] ~ 0 ;                                    MONT0039
START1:       LASTWALK ~ ROW ;     G ~ 1 ;                              MONT0040
START2 :      RANDOM(RN) ;    WALK ~ ENTIER(RN|D+1) ;                   MONT0041
              IF WALK > N THEN GO TO STOP ;                             MONT0042
              G ~ V[LASTWALK, WALK] | G ;                               MONT0043
COMMENT       A STEP OF A RANDOM WALK WAS TAKEN.  IF A STOP WAS         MONT0044
              ENCOUNTERED CONTROL WOULD BE TRANSFERRED TO THE LOCAL     MONT0045
              LABEL "STOP".  OTHERWISE THE PAYOFF "G" IS UPGRADED AND   MONT0046
              ANOTHER STEP OF THE WALK TAKEN. ;                         MONT0047
              LASTWALK ~ WALK ;                                         MONT0048
              GO TO START2 ;                                            MONT0049
STOP:         COUNT ~ COUNT + 1 ;                                       MONT0050
COMMENT       "COUNT" TALLIES THE NUMBER OF RANDOM WALKS COMPLETED ;    MONT0051
              SUM[LASTWALK] ~ SUM[LASTWALK] + G ;                       MONT0052
COMMENT       THE "SCORE" OF THE WALK IS AWARDED TO THE APPROPRIATE     MONT0053
              ELEMENT ;                                                 MONT0054
              IF COUNT < NWK THEN GO TO START1 ;                        MONT0055
COMMENT       IF "COUNT" IS A MULTIPLE OF 1000 THE SCORES FOR EACH      MONT0056
              ELEMENT ARE MULTIPLIED BY N(THE INVERSE OF THE STOP       MONT0057
              PROBABILITY), AND AVERAGED.  THIS AVERAGE CONVERGES TO    MONT0058
              THE VALUE OF THE DESIRED INVERSE ELEMENT ;                MONT0059
              FOR K ~ 1 STEP 1 UNTIL N DO                               MONT0060
              INV[K] ~ N | SUM[K] / COUNT ;                             MONT0061
COMMENT       A TEST IS MADE TO DETERMINE IF THE APPROXIMATION IS       MONT0062
              CLOSE ENOUGH ;                                            MONT0063
              FOR I ~ 1 STEP 1 UNTIL N DO                               MONT0064
              FOR K ~ 1 STEP 1 UNTIL N DO                               MONT0065
              TEST[I] ~ INV[K] | A[K,I] + TEST[I] ;                     MONT0066
              FOR I ~ 1 STEP 1 UNTIL ROW-1, ROW+1 STEP 1 UNTIL N DO     MONT0067
              RES ~ ABS(TEST[I]) + RES ;                                MONT0068
              RES ~ ABS(TEST[ROW] -1.0) + RES ;                         MONT0069
              IF RES < TOL THEN GO TO EXIT ;                            MONT0070
              IF COUNT } 1000 | MXM THEN GO TO EXIT ;                   MONT0071
COMMENT       IF THE RESULTS ARE NOT SUFFICIENTLY ACCURATE AFTER "MXM"  MONT0072
              | 1000 WALKS, AN EXIT FROM THE PROCEDURE IS MADE ;        MONT0073
              NWK ~ NWK + 1000 ;      RES ~ 0 ;                         MONT0074
              FOR K ~ 1 STEP 1 UNTIL N DO                               MONT0075
              TEST[K] ~ 0 ;                                             MONT0076
              GO TO START1 ;                                            MONT0077
EXIT:    END ;                                                          MONT0078
