 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          VRCL0001
COMMENT       VECTOR REPRESENTATION OF RIGID BODY ROTATION              VRCL0002
                                                                        VRCL0003
              JAMES S. W. WONG                                          VRCL0004
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)      VRCL0005
                                                                        VRCL0006
              CARD SEQUENCE BEGINS WITH VRCL-0001                       VRCL0007
              FIRST RELEASE DATE  09-15-63 ;                            VRCL0008
                                                                        VRCL0009
REAL          X,Y,Z,X1,Y1,Z1,EX,EY,EZ,CT,ST,THETA,DELTAT,GAMMA,DELTAG,  VRCL0010
              DP,BETA,DELTAB,NORM,MAXTHETA,LENG,PX,PY,PZ,GAMO,BETO  ;   VRCL0011
                                                                        VRCL0012
INTEGER       I,J,K,N,P,COUN,SW1,SW2,M  ;                               VRCL0013
                                                                        VRCL0014
FILE IN       FIN1(1,10) ;                                              VRCL0015
                                                                        VRCL0016
FORMAT IN     FMIN1(5I5),                                               VRCL0017
              FMIN2(5F10.5) ;                                           VRCL0018
                                                                        VRCL0019
FILE OUT      FOUT1(1,15) ;                                             VRCL0020
                                                                        VRCL0021
FORMAT OUT    FMOUT1(4(X20,F10.5)) ,                                    VRCL0022
              FMOUT2(X20,"BETA",F10.5) ,                                VRCL0023
              FMOUT3(X20,"GAMMA",F10.5) ,                               VRCL0024
              FMOUT4(X20,"EX",F10.5,X20,"EY",F10.5,X20,"EZ",F10.5) ;    VRCL0025
                                                                        VRCL0026
LABEL         L1,L2,L3,L4,L5 ;                                          VRCL0027
                                                                        VRCL0028
              READ(FIN1,FMIN1,N,M,SW1,P,SW2) ;                          VRCL0029
              COUN ~ 0 ;                                                VRCL0030
L1:           READ(FIN1,FMIN2,EX,EY,EZ,DELTAG,DELTAB) ;                 VRCL0031
              READ(FIN1,FMIN2,X,Y,Z,DELTAT,MAXTHETA) ;                  VRCL0032
              IF SW1=1 THEN GO TO L4 ;                                  VRCL0033
              LENG ~ SQRT(X|X + Y|Y + Z|Z) ;                            VRCL0034
              PX ~ SQRT(Y|Y + Z|Z) ;                                    VRCL0035
              PY ~ SQRT(X|X + Z|Z) ;                                    VRCL0036
              PZ ~ SQRT(X|X + Y|Y) ;                                    VRCL0037
              GAMO ~ ARCTAN(PZ/Z) ;                                     VRCL0038
              BETO ~ ARCTAN(PY/Y) ;                                     VRCL0039
                                                                        VRCL0040
              FOR I ~ 0 STEP 1 UNTIL N DO                               VRCL0041
         BEGIN                                                          VRCL0042
              GAMMA ~ I | DELTAG + GAMO ;                               VRCL0043
                                                                        VRCL0044
              FOR J ~ 0 STEP 1 UNTIL M DO                               VRCL0045
         BEGIN                                                          VRCL0046
              BETA ~ J | DELTAB + BETO ;                                VRCL0047
                                                                        VRCL0048
L2:           CT ~ COS(THETA) ;                                         VRCL0049
              ST ~ SIN(THETA) ;                                         VRCL0050
              Z ~ COS(GAMMA) ;                                          VRCL0051
              Y ~ COS(BETA) ;                                           VRCL0052
              X ~ SQRT(1 - Z|Z - Y|Y) ;                                 VRCL0053
                                                                        VRCL0054
L4:           DP ~ EX|X + EY|Y + EZ|Z ;                                 VRCL0055
              X1 ~ X|CT + (1-CT)|EX|DP + ST|(Z|EY-Y|EZ) ;               VRCL0056
              Y1 ~ Y|CT + (1-CT)|EY|DP + ST|(X|EZ-Z|EX) ;               VRCL00-7
              Z1 ~ Z|CT + (1-CT)|EZ|DP + ST|(Y|EX-X|EY) ;               VRCL0058
              IF SW2=0 THEN GO TO L5 ;                                  VRCL0059
              NORM ~ SQRT( X1|X1 + Y1|Y1 + Z1|Z1 ) ;                    VRCL0060
              X1 ~ X1/NORM ;                                            VRCL0 61
              Y1 ~ Y1/NORM ;                                            VRCL0062
              Z1 ~ Z1/NORM ;                                            VRCL0063
                                                                        VRCL0064
L5:           WRITE ( FOUT1,FMOUT1,X,Y,Z,THETA,X1,Y1,Z1 ) ;             VRCL0065
              THETA ~ THETA + DELTAT ;                                  VRCL0066
              IF THETA > MAXTHETA THEN GO TO L3 ELSE GO TO L2 ;         VRCL0067
                                                                        VRCL0068
L3:           WRITE ( FOUT1,FMOUT2,BETA ) ;                             VRCL0069
              THETA ~ 0 ;                                               VRCL0070
         END;                                                           VRCL0071
              WRITE ( FOUT1,FMOUT3,GAMMA ) ;                            VRCL0072
              BETA ~ 0 ;                                                VRCL0073
         END;                                                           VRCL0074
              COUN ~ COUN + 1 ;                                         VRCL0075
              WRITE ( FOUT1,FMOUT4,EX,EY,EZ ) ;                         VRCL0076
              IF COUN < P THEN GO TO L1 ;                               VRCL0077
         END.                                                           VRCL0078

 LABEL  000000000FILE1  0010000001
    2    3    0    3    0
   1.0       0.0       0.0       0.1       0.1
   1.0       2.0       3.0       0.1       0.5
   0.0       0.0       1.0       0.1       0.1
   1.0       2.0       3.0       0.1       0.5
   0.0       1.0       0.0       0.1       0.1
   1.0       2.0       3.0       0.1       0.5

