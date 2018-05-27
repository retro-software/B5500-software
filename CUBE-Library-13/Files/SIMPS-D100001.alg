BEGIN                                                                   00000100
COMMENT        PROGRAM - SIMPSONS RULE,                                 00000200
               CUBE LIBRARY NUMBER IS D100001.                          00000300
               THIS VERSION DATED 6/19/67;                              00000400
COMMENT                                                                 00000500
                         "SIMPSON-S  RULE"**                            00000600
                                                                        00000700
DISCLAIMER:  THE AUTHORS OF THIS PROGRAM, THE CUBE ORGANIZATION,        00000800
     AND THE BURROUGHS CORPORATION, BELIEVE THIS PROGRAM MATERIAL       00000900
     TO BE CORRECT, HOWEVER, THEY BEAR NO RESPONSIBILITY,               00001000
     FINANCIAL OR OTHERWISE, FOR ERROR RESULTING FROM ITS USE, NOR      00001100
     ACCEPT ANY RESPONSIBILITY FOR ITS MAINTENANCE.                     00001200
                                                                        00001300
AUTHOR                                                                  00001400
     TRANSLITERATED BY PAUL DAVIS  UNIVERSITY OF VIRGINIA               00001500
                                                                        00001600
ABSTRACT                                                                00001700
     SIMPSON-S RULE INTEGRATION OF SINGLE OR MULTIPLE INTEGRALS         00001800
                                                                        00001900
SYSTEM                                                                  00002000
     MINIMUM B5000                                                      00002100
                                                                        00002200
                                                                        00002300
                                                                        00002400
     SIMPS IS A TYPE REAL PROCEDURE WHICH CALCULATES THE VALUE OF A     00002500
SINGLE INTEGRAL BY SIMPSON"S RULE. THE PROCEDURE IS WRITTEN IN SUCH A   00002600
WAY THAT IT CAN BE CALLED RECURSIVELY FOR THE EVALUATION OF AN ITERATED 00002700
INTEGRAL.                                                               00002800
                                                                        00002900
     THE PROCEDURE CALL IS                                              00003000
               SIMPS (X,X1,X2,DELTA,F)                                  00003100
WHERE THE INTEGRATION OF THE REAL EXPRESSION F IS TO BE TAKEN OVER THE  00003200
REAL VARIABLE X BETWEEN THE LIMITS X1 AND X2, WHERE X1 AND X2 MAY BE    00003300
EXPRESSIONS, AND WITHIN A RELATIVE TOLERANCE DELTA.                     00003400
FOR EXAMPLE THE CALL                                                    00003500
          M~SIMPS(X,0,1,@-8,SIMPS(Y,0,SQRT(1-X*2),@-10,X*2+Y*2))        00003600
WAS USED TO EVALUATE THE DOUBLE INTEGRAL OF                             00003700
          F(X)=X*2+Y*2                                                  00003800
FOR Y=0 TO Y=SQRT(1-X*2) AND X=0 TO X=1. ANALYTICALLY ONE OBTAINS       00003900
M=.392699. THE PROCEDURE SIMPS PRODUCED M=.3926978 IN 9.35 SECONDS.     00004000
                                                                        00004100
**   TRANSLITERATED FROM THE ALGORITHM SECTION OF THE "COMMUNICATIONS OF00004200
     THE ACM", VOL.7, NUMBER 6,JUNE 1964.                               00004300
                                                                        00004400
************************************************************************00004500
                                                                        00004600
                                                                        00004700
                                                                        00004800
                                                                       ;00004900
     FILE OUT LP 1(1,15);                                               00005000
     FORMAT F(F12.7);                                                   00005100
     REAL X,Y,Z;                                                        00005200
REAL PROCEDURE SIMPS(X,X1,X2,DELTA,F);                                  00005300
          VALUE X1,X2,DELTA;  REAL X,X1,X2,DELTA,F;                     00005400
     BEGIN                                                              00005500
          BOOLEAN TURNING;  LABEL BOX,BOX2;                             00005600
          REAL Z1,Z2,Z3,H,K;                                            00005700
          TURNING~FALSE;                                                00005800
          IF X1=X2 THEN                                                 00005900
               BEGIN                                                    00006000
                    Z1~0;                                               00006100
                    GO TO BOX2                                          00006200
               END;                                                     00006300
          IF X1>X2 THEN                                                 00006400
               BEGIN                                                    00006500
                    H~X1;                                               00006600
                    X1~X2;                                              00006700
                    X2~H;                                               00006800
                    TURNING~TRUE;                                       00006900
               END;                                                     00007000
          X~X1;  Z1~F;  X~X2;  Z3~Z1~Z1+F;                              00007100
          K~X2-X1;                                                      00007200
BOX:                                                                    00007300
          Z2~0;  H~K/2;                                                 00007400
          FOR X~X1+H STEP K UNTIL X2 DO Z2~Z2+F;                        00007500
          Z1~Z1+4|Z2;                                                   00007600
          IF H|ABS((Z1-2|Z3)/(IF Z1=0 THEN 1.0 ELSE Z1))<DELTA          00007700
               THEN GO TO BOX2 ELSE Z3~Z1;                              00007800
          Z1~Z1-2|Z2;                                                   00007900
          K~H;                                                          00008000
          GO TO BOX;                                                    00008100
BOX2:                                                                   00008200
          IF TURNING THEN H~-H;                                         00008300
          SIMPS~H|Z1/3                                                  00008400
     END OF SIMPS;                                                      00008500
     WRITE(LP,F,                                                        00008600
            SIMPS(X,0,1,@-8,SIMPS(Y,0,SQRT(1-X*2),@-10,X*2+Y*2))   );   00008700
END.                                                                    00008800
