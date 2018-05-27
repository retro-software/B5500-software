
         BEGIN                                                          CMNT0001
COMMENT       NEWTON FORWARD AND BACKWARD INTERPOLATION PROCEDURE       CMNT0002

              J. K. KONDO                                               CMNT0003
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           CMNT0004

              PROCEDURE CARD SEQUENCE STARTS WITH NFBI0001.             CMNT0005
              FIRST RELEASE 06-28-1963  ;                               CMNT0006


PROCEDURE     INTERP(XO,XF,H,Y,X,BETA,EPSILON,M,INW,LLABEL)  ;          NFBI0001
VALUE         XO,XF,H,X,BETA,EPSILON,M  ;                               NFBI0002
INTEGER       M  ;                                                      NFBI0003
REAL          XO,XF,H,X,EPSILON,INW  ;                                  NFBI0004
BOOLEAN       BETA  ;                                                   NFBI0005
ARRAY         Y[0]  ;                                                   NFBI0006
         BEGIN                                                          NFBI0008
LABEL         LLABEL  ;                                                 NFBI0007
INTEGER       N,I,J,K  ;                                                NFBI0009
REAL          U1,U,TEMP  ;                                              NFBI0010
LABEL         NFOR,L1,NBAK,L2,EOP  ;                                    NFBI0011
              IF (X < (XO - 2|H)) OR (X > (XF + 2|H)) THEN GO TO LLABEL;NFBI0012
              N ~ ABS((XF - XO)/H)  ;  IF M > N THEN M ~ N  ;           NFBI0013
              U1 ~ (XF + XO)/2.0  ;  K ~ 1  ;                           NFBI0014
              IF X < (U1 - 5 | H) THEN GO TO NFOR ELSE                  NFBI0015
              IF (X > U1 + 5 | H) OR BETA THEN GO TO NBAK  ;            NFBI0016
NFOR:         J ~ M - 1  ;  U ~ (X - XO)/H  ;                           NFBI0017
              INW ~ Y[0]  ;  U1 ~ 1.0  ;                                NFBI0018
L1:           Y[0] ~ Y[1] - Y[0]  ;                                     NFBI0019
              IF ABS(Y[0]) < EPSILON THEN GO TO EOP  ;                  NFBI0020
              U1 ~ U1|(U-K+1.0)/K  ;                                    NFBI0021
              INW ~ INW + U1 | Y[0]  ;                                  NFBI0022
              IF K = M THEN GO TO EOP  ;                                NFBI0023
              FOR I ~ 1 STEP 1 UNTIL J DO Y[I] ~ Y[I+1] - Y[I]  ;       NFBI0024
              J ~ J - 1  ;  K ~ K + 1  ;                                NFBI0025
              GO TO L1  ;                                               NFBI0026
NBAK:         U ~ (X - XF)/H  ;   J ~ N - M + 1  ;                      NFBI0027
              INW ~ Y[N]  ;  U1 ~ 1.0  ;                                NFBI0028
L2:           Y[N] ~ Y[N] - Y[N-1]  ;                                   NFBI0029
              IF ABS(Y[N]) < EPSILON THEN GO TO EOP  ;                  NFBI0030
              U1 ~ U1 | (U + K - 1.0) / K  ;                            NFBI0031
              INW ~ INW + Y[N] | U1  ;                                  NFBI0032
              IF K = M THEN GO TO EOP  ;                                NFBI0033
              FOR I ~ N-1 STEP -1 UNTIL J DO  Y[I] ~ Y[I] - Y[I-1]  ;   NFBI0034
              J ~ J + 1  ;  K ~ K + 1  ;                                NFBI0035
              GO TO L2  ;                                               NFBI0036
EOP:          END  ;                                                    NFBI0037
         END  .                                                         CMNT0007


    .6800    .6900  .0005 20
  .80866138  .80948868  .81031665  .81114529  .81197460  .81280459  .81363526
  .81446660  .81529861  .81613131  .81696468  .81779874  .81863348  .81946890
  .82030501  .82114180  .82197928  .82281745  .82365631  .82449586  .82533611
    .6807FALSE  .0000001  20
    .6796FALSE  .00000001 20
    .6902 TRUE  .0000001  20
    .9550    .9650  .0005 20
  .81631374  .81660244  .81689094  .81717924  .81746732  .81775521  .81804289
  .81833037  .81861764  .81890471  .81919157  .81947823  .81976468  .82005093
  .82033697  .82062281  .82090844  .82119387  .82147909  .82176410  .82204892
    .9543FALSE  .00000001 20
    .9552FALSE  .00000001 20
    .9598 TRUE  .00000001 20
    .9646 TRUE  .00000001 20

