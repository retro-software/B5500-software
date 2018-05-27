REAL PROCEDURE INTEGRAL (F)"LIMITS"(A,B)"TOLERANCE"(EPS)  ;             SIMP0001
VALUE         A,B,EPS   ;                                               SIMP0002
REAL PROCEDURE F  ;                                                     SIMP0003
REAL          A,B,EPS  ;                                                SIMP0004
                                                                        SIMP0005
         BEGIN                                                          SIMP0006
                                                                        SIMP0007
INTEGER       LEVEL  ;                                                  SIMP0008
                                                                        SIMP0009
REAL PROCEDURE SIMPSON (F,A,DA,FA,FM,FB,ABSAREA,EST,EPS,LEVEL)  ;       SIMP0010
                                                                        SIMP0011
VALUE         A,DA,FA,FM,FB,EST,EPS   ;                                 SIMP0012
REAL PROCEDURE F  ;                                                     SIMP0013
REAL          A,DA,FA,FM,FB,ABSAREA,EST,EPS  ;                          SIMP0014
INTEGER       LEVEL  ;                                                  SIMP0015
                                                                        SIMP0016
         BEGIN                                                          SIMP0017
                                                                        SIMP0018
REAL          DX,X1,X2,EST1,EST2,EST3,F1,F2,F3,F4,SUM,EPS1  ;           SIMP0019
                                                                        SIMP0020
              DX ~ DA|(0.33333333333)  ;  X1 ~ A+DX  ;  X2 ~ X1+DX  ;   SIMP0021
              F1 ~ (4.0)|F(A+DX|(0.5))  ;  F2 ~ F(X1)  ;                SIMP0022
              F3 ~ F(X2)  ;  F4 ~ (4.0)|F(A+(2.5)|DX)  ;                SIMP0023
              EST1 ~ (FA+F1+F2)|DX|(0.16666666667)  ;                   SIMP0024
              EST2 ~ (F2+FM+F3)|DX|(0.16666666667)  ;                   SIMP0025
              EST3 ~ (F3+F4+FB)|DX|(0.16666666667)  ;                   SIMP0026
              ABSAREA ~ ABSAREA-ABS(EST)+ABS(EST1)+ABS(EST2)+ABS(EST3) ;SIMP0027
              SUM ~ EST1+EST2+EST3  ;  LEVEL ~ LEVEL+1  ;               SIMP0028
              EPS1 ~ EPS|(0.33333333333)  ;                             SIMP0029
                                                                        SIMP0030
              SIMPSON ~ IF (ABS(EST-SUM) { EPS|ABSAREA AND EST ! 1) OR  SIMP0031
                           LEVEL } 7 THEN SUM ELSE                      SIMP0032
                   SIMPSON (F,A ,DX,FA,F1,F2,ABSAREA,EST1,EPS1,LEVEL)   SIMP0033
                 + SIMPSON (F,X1,DX,F2,FM,F3,ABSAREA,EST2,EPS1,LEVEL)   SIMP0034
                 + SIMPSON (F,X2,DX,F3,F4,FB,ABSAREA,EST3,EPS1,LEVEL)  ;SIMP0035
              LEVEL ~ LEVEL-1                                           SIMP0036
         END          ;                                                 SIMP0037
                                                                        SIMP0038
              LEVEL ~ 1  ;                                              SIMP0039
                                                                        SIMP0040
              INTEGRAL ~ SIMPSON (F,A,B-A,F(A),(4.0)|F((A+B)|(0.5)),F(B)SIMP0041
                                   ,1.0,1.0,EPS,LEVEL)                  SIMP0042
         END           ;                                                SIMP0043
