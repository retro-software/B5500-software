
         BEGIN                                                          CMNT0001
COMMENT       EVERETT INTERPOLATION PROCEDURE                           CMNT0002

              JOANNE K. KONDO                                           CMNT0003
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           CMNT0004

              PROCEDURE CARD SEQUENCE STARTS WITH EVER0001.             CMNT0005
              FIRST RELEASE 06/28/1963  ;                               CMNT0006


PROCEDURE     EVERETT(XI,XF,X,H,Y,EPSILON,M,IE,LLABEL)  ;               EVER0001
VALUE         XI,XF,X,H,EPSILON  ;                                      EVER0002
INTEGER       M  ;                                                      EVER0003
REAL          XI,XF,X,H,EPSILON,IE  ;                                   EVER0004
ARRAY         Y[0]  ;                                                   EVER0005
LABEL         LLABEL  ;                                                 EVER0006
         BEGIN                                                          EVER0007
INTEGER       I,N,P,IV,FV,BOOL,J,L  ;                                   EVER0008
REAL          XO,EO,EI,TEMP1,TEMP2,U,T  ;                               EVER0009
LABEL         LOOP,EXIT  ;                                              EVER0010
DEFINE        DIFFERENCE = FOR P ~ IV STEP 1 UNTIL FV-L DO              EVER0011
              Y[P] ~ Y[P+1] - Y[P]#  ;                                  EVER0012
              IF (X < XI) OR (X > XF) THEN GO TO LLABEL  ;              EVER0013
              N ~ (XF-XI)/H  ;  BOOL ~ 0  ;                             EVER0014
              J ~ ENTIER(N/2)  ;                                        EVER0015
              IF M > N THEN M ~ N  ;                                    EVER0016
              IF M MOD 2 = 1 THEN M ~ M - 1  ;                          EVER0017
              IF M } N - 1  THEN                                        EVER0018
         BEGIN                                                          EVER0019
              BOOL ~ (N+1) MOD 2  ;  IV ~ 0  ;  FV ~  N                 EVER0020
         END                                                            EVER0021
              ELSE                                                      EVER0022
         BEGIN                                                          EVER0023
              IV ~ J - ENTIER(M/2)  ;  FV ~ J + ENTIER(M/2) + 1         EVER0024
         END  ;                                                         EVER0025
              XO ~ XI + J|H  ;                                          EVER0026
              EI ~ U ~ (X-XO)/H  ;  T ~ EO ~ 1.0- U  ;                  EVER0027
              IE ~ EO|Y[J] + EI|Y[J+1]  ;                               EVER0028
              I ~ L ~ 1  ;                                              EVER0029
LOOP:         DIFFERENCE  ;  L ~ L + 1  ;                               EVER0030
              TEMP1 ~ Y[J-I+1] - Y[J-I]  ;                              EVER0031
              TEMP2 ~ Y[J-I+2] - Y[J-I+1]  ;                            EVER0032
              IF (ABS(TEMP1) < EPSILON) AND (ABS(TEMP2) < EPSILON) THEN EVER0033
              GO TO EXIT  ;                                             EVER0034
              EO ~ EO|(T+I)|(T-I)/(4|I|I + 2|I)  ;                      EVER0035
              IF (ABS(TEMP2) < EPSILON) OR (L = M AND BOOL = 1) THEN    EVER0036
         BEGIN                                                          EVER0037
              IE ~ IE + EO| TEMP1  ;  GO TO EXIT                        EVER0038
         END  ;                                                         EVER0039
              EI ~ EI|(U+I)|(U-I)/(4|I|I + 2|I)  ;                      EVER0040
              IF ABS(TEMP1) < EPSILON THEN                              EVER0041
         BEGIN                                                          EVER0042
              IE ~ IE + EI| TEMP2  ;  GO TO EXIT                        EVER0043
         END  ;                                                         EVER0044
              IE ~ IE + EO|TEMP1 + EI|TEMP2  ;                          EVER0045
              IF L = M  THEN GO TO EXIT  ;                              EVER0046
              DIFFERENCE  ;  L ~ L + 1  ;  I ~ I + 1  ;                 EVER0047
              GO TO LOOP  ;                                             EVER0048
EXIT:    END  ;                                                         EVER0049
         END  .                                                         CMNT0007


              TEST CASE DATA.
    .9550    .9650  .0005 20
  .81631374  .81660244  .81689094  .81717924  .81746732  .81775521  .81804289
  .81833037  .81861764  .81890471  .81919157  .81947823  .81976468  .82005093
  .82033697  .82062281  .82090844  .82119387  .82147909  .82176410  .82204892
    .9571 .00000001 20
    .9598 .0000001  20
    .9602 .00000001 20
    .9632 .00000001 20
    .6800    .6900  .0005 20
  .80866138  .80948868  .81031665  .81114529  .81197460  .81280459  .81363526
  .81446660  .81529861  .81613131  .81696468  .81779874  .81863348  .81946890
  .82030501  .82114180  .82197928  .82281745  .82365631  .82449586  .82533611
    .6887 .0000001  20
    .6852 .0000001  20
    .6847 .0000001  20
