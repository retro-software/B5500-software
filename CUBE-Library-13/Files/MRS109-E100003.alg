
         BEGIN                                                          CMNT0001
COMMENT       NEWTON FORWARD AND BACKWARD INTERPOLATION PROCEDURE       CMNT0002
              J. K. KONDO                                               CMNT0003
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION) .          CMNT0004

              PROCEDURE CARD SEQUENCE STARTS WITH NFBI0001.             CMNT0005
              FIRST RELEASE 06-28-1963  ;                               CMNT0006

PROCEDURE     INBES(XI,XF,H,Y,X,EPSILON,M,IB,LLABEL)  ;                 IBES0002
VALUE         XI,XF,H,X,EPSILON  ;                                      IBES0003
INTEGER       M  ;                                                      IBES0004
REAL          XI,XF,H,X,EPSILON,IB  ;                                   IBES0005
ARRAY         Y[0]  ;                                                   IBES0006
LABEL         LLABEL  ;                                                 IBES0007
         BEGIN                                                          IBES0008
INTEGER       I,J,K,N,P,IV,FV,L  ;                                      IBES0009
REAL          XO, U2, U, U1, TEMP, TRY  ;                               IBES0010
LABEL         LA,ENDBES,TEST  ;                                         IBES0011
              IF (X < XI) OR (X > XF)  THEN GO TO LLABEL  ;             IBES0012
              N ~ (XF-XI)/H  ;                                          IBES0013
              IF M } N THEN M ~ IF N MOD 2 = 0 THEN N -1 ELSE N  ;      IBES0014
              J ~ ENTIER(N/2)  ;                                        IBES0015
              IV ~ J - ENTIER(M/2)  ;  FV ~ J + ENTIER(M/2) + 1  ;      IBES0016
              XO ~ XI + J| H  ;                                         IBES0017
              U2 ~ U ~ (X - XO)/H  ;  U1 ~ U - 0.5  ;                   IBES0018
              L ~ 2  ;  I ~ 1  ;  K ~ 1  ;                              IBES0019
              IB ~ Y[J]  ;                                              IBES0020
              FOR P ~ IV STEP 1 UNTIL FV-1 DO  Y[P] ~ Y[P+1] - Y[P]  ;  IBES0021
              IB ~ IB + U2 | Y[J]  ;                                    IBES0022
              TRY ~ U2 ~ 0.5|U2|(U - 1)  ;                              IBES0023
              TEMP ~ 0.5|(Y[J+1] - Y[J-1])  ;                           IBES0024
              GO TO TEST  ;                                             IBES0025
LA:           IF K = 1 THEN                                             IBES0026
         BEGIN                                                          IBES0027
              TEMP ~ 0.5|(Y[J-I+1] - Y[J-I-1])  ;                       IBES0028
              I ~ I + 1  ;                                              IBES0029
              TRY ~ U2 ~ U2|(U-I)/L  ;                                  IBES0030
         END                                                            IBES0031
              ELSE                                                      IBES0032
         BEGIN                                                          IBES0033
              TEMP ~ Y[J-I+1] - Y[J-I]  ;                               IBES0034
              U2 ~ U2/L  ;  TRY ~ U2|U1  ;                              IBES0035
         END  ;                                                         IBES0036
TEST:         IF ABS(TEMP) < EPSILON THEN GO TO ENDBES  ;               IBES0037
              IB ~ IB + TEMP | TRY  ;                                   IBES0038
              IF L = M THEN GO TO ENDBES  ;                             IBES0039
              FOR P ~ IV STEP 1 UNTIL FV-L DO Y[P] ~ Y[P+1] - Y[P]  ;   IBES0040
              K ~ -K  ;  L ~ L + 1  ;  GO TO LA  ;                      IBES0041
ENDBES:  END  ;                                                         IBES0042

              TEST CASE DATA.
    .6800    .6900  .0005 20
  .80866138  .80948868  .81031665  .81114529  .81197460  .81280459  .81363526
  .81446660  .81529861  .81613131  .81696468  .81779874  .81863348  .81946890
  .82030501  .82114180  .82197928  .82281745  .82365631  .82449586  .82533611
    .6852  .0000001 22
    .6846  .0000001 20
    .9550    .9650  .0005 20
  .81631374  .81660244  .81689094  .81717924  .81746732  .81775521  .81804289
  .81833037  .81861764  .81890471  .81919157  .81947823  .81976468  .82005093
  .82033697  .82062281  .82090844  .82119387  .82147909  .82176410  .82204892
    .9598  .0000001 20
    .9646  .0000001 20
