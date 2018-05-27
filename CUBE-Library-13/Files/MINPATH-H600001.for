FILE  5 = LINKIN, UNIT=READER                                           00000100
FILE  6 = LINKOUT, UNIT = PRINTER                                       00000200
C                                                                       00000300
C MINIMUM PATH ALGORITHM DEVELOPED BY LEON MILLER AND JOHN KIESER AT    00000400
C INTERNATIONAL BANK FOR RECONSTRUCTION AND DEVELOPMENT. WASHINGTON D.C.00000500
C APRIL 1969.                                                           00000600
C     THE MINIMUM PATH PROGRAM IN FORTRAN IV HAS BEEN DESIGNED WITH     00000700
C  SEVERAL OPTIONS FOR THE USER. IT IS POSSIBLE TO COMPUTE MINIMUM PATHS00000800
C  FOR ALL NODES IN A TRANSPORT NETWORK OR ONLY FOR SELECTED NODES. THE 00000900
C  USER CAN SPECIFY THE PRINTING OF THE LINK (COST/DISTANCE) ARRAY WHICH00001000
C  IS THE INPUT TO THE PROGRAM. HE CAN ALSO INDICATE WHETHER OR NOT THE 00001100
C  NODE TO NODE ROUTES SHOULD BE CALCULATED AND PRINTED. EACH OF THESE  00001200
C  OPTIONS WILL AFFECT THE OUTPUT AND THE RUNNING TIME OF THE PROGRAM.  00001300
C  PROGRAM EFFICIENCY CAN BE IMPROVED WHEN THE USER REDIMENSIONS THE L  00001400
C (LINK) AND X1 (MINIMUM PATH)  ARRAYS ACCORDING TO THE SPECIFICATIONS  00001500
C  OF THE PROBLEM. FOR EXAMPLE THE USER WOULD WANT TO REDIMENSION HIS   00001600
C  ARRAYS AND RECOMPILE THE PROGRAM IF A NEW NETWORK CONTAINED 50 NODES 00001700
C  AND HE HAD BEEN PREVIOUSLY UTILIZING THE PROGRAM FOR A 160 NODE NET- 00001800
C  WORK. THIS STEP CAN BE EASILY ACCOMPLISHED BY MODIFYING THE TWO      00001900
C  COMMON STATEMENTS. THE MAXIMUM NUMBER OF NODES THE PROGRAM CAN       00002000
C  HANDLE ON THE B5500 IS 176.                                          00002100
C                                                                       00002200
C     THE INPUT IS ENTERED FOR EACH PROBLEM IN THE FORM OF PUNCHED      00002300
C  CARDS. THE FIRST CARD CONTAINS THE LINK ARRAY SIZE BY ROW AND COLUMN,00002400
C  THE PROBLEM NUMBER AND INTEGER SWITCHS WHICH INDICATE WHETHER A      00002500
C  MINIMUM ROUTE IS TO BE DETERMINED, WHETHER MINIMUM PATHS WILL BE     00002600
C  COMPUTED ONLY FOR CERTAIN NODES AND WHETHER THE INPUT, LINK ARRAY,   00002700
C  SHOULD BE PRINTED. THE FORMAT OF THIS PARAMETER CARD IS AS FOLLOWS   00002800
C     COLUMN 1 - 3 NUMBER OF ROWS  -  IROW                              00002900
C     COLUMN 4 - 6 NUMBER OF COLUMNS  -  ICOL                           00003000
C     COLUMN 7 - 8 PROBLEM NUMBER  -  PROB                              00003100
C     COLUMN 9  MINIMUM ROUTE  -  KSHORT                                00003200
C     COLUMN 10 - INDIVIDUAL NODE PROCESSING SWITCH INODE               00003300
C     COLUMN 11 - INPUT PRINT SWITCH  INPRNT                            00003400
C          0 = OFF   1 = ON.                                            00003500
C                                                                       00003600
C     THE SECOND INPUT IS A SET OF CARDS CONTAINING THE ELEMENTS OF THE 00003700
C  LINK ARRAY. THE INITIAL CARD OF THE SET MUST CONTAIN $MILLER IN      00003800
C  COLUMNS 2 - 8. THE NEXT CARD MUST CONTAIN THE NAME OF THE LINK ARRAY,00003900
C  L, AN EQUAL SIGN AND ARRAY ELEMENTS. THIS IS THE NAMELIST METHOD OF  00004000
C  INPUT. THE ELEMENTS CAN BE ENTERED IN TWO WAYS. AN EXAMPLE OF THE    00004100
C  FIRST WOULD BE L = 1.,2.,3., ..... AND AN EXAMPLE OF THE SECOND IS   00004200
C  L (1,1) = 1., L(2,1) = 2., L(3,1) = 3.  IN THE FIRST WAY THE ELEMENTS00004300
C  ARE ENTERED AS A WHOLE ARRAY COLUMN BY COLUMN. IN THE SECOND WAY THE 00004400
C  ELEMENTS ARE ENTERED INDIVIDUALLY. NOTE THAT L(3,1) INDICATES THE    00004500
C  DISTANCE, TIME, COST, ETC., FROM NODE 3 TO NODE 1. THIS WOULD BE THE 00004600
C PREFERRED METHOD IF THERE WERE MANY ZERO ENTRIES. THE LAST ELEMENT    00004700
C  MUST BE FOLLOWED BY A $ SIGN SUCH AS L (3,4) = 5.$ OR .5$. NOTE THAT 00004800
C  A COMMA MUST BE PUNCHED BETWEEN EACH ELEMENT, AND EACH ELEMENT MUST  00004900
C  BE ENTERED WITH A DECIMAL POINT. SEVERAL PROBLEMS CAN BE RUN AT THE  00005000
C  SAME TIME USING THIS TECHNIQUE. A LARGE ARRAY OF LINK COSTS, TRAVEL  00005100
C  TIMES OR DISTANCES COULD BE ENTERED INTO THE PROGRAM AND THEN        00005200
C  INDIVIDUAL LINKS COULD BE MODIFIED AND THE PROGRAM RERUN AS MANY     00005300
C  TIMES AS DESIRED. THE PROGRAM IS DESIGNED TO CHECK FOR ADDITIONAL    00005400
C  INPUT AFTER EACH PROBLEM IS SOLVED. THE ORDER OF INPUT IS REPEATED   00005500
C  FOR EACH PROBLEM. THE LINK ARRAY ALWAYS REMAINS IN MEMORY UNTIL THE  00005600
C  INPUT IS EXHAUSTED. IF THE INPRNT SWITCH IS ON, THE THIRD INPUT CARD 00005700
C  SHOULD BE A FORTRAN FORMAT STATEMENT WITH THE LEFT PARENTHESIS IN    00005800
C  COLUMN 1 AND THE RIGHT PARENTHESIS PUNCHED BEFORE OR ON COLUMN 60.   00005900
C  THIS CARD PROVIDES THE FORMAT FOR PRINTING OUT THE LINK ARRAY.       00006000
C                                                                       00006100
C     IF THE INODE SWITCH IS ON, THE NEXT CARDS SHOULD CONTAIN THE      00006200
C  NUMBERS OF THE NODES FOR WHICH MINIMUM PATHS WILL BE COMPUTED. THESE 00006300
C  WOULD APPEAR IN COLUMNS 1 - 3 IN AN I FORMAT.                        00006400
C                                                                       00006500
C     THE OUTPUT OF THE PROGRAM IS A LIST OF MINIMUM PATHS, NODE TO NODE00006600
C  ROUTES AND THE LINK ARRAY IN THE COMBINATION SPECIFIED ON THE INPUT  00006700
C  PARAMETER CARD.                                                      00006800
C    THE FOLLOWING IS A SAMPLE INPUT STREAM FOR THREE PROBLEMS WITH     00006900
C    DIFFERENT OPTIONS SPECIFIED. **NOTE** THAT WHEN THE NAMELIST INPUT 00007000
C    IS ENTERED AS IN PROBLEM ONE THE L ARRAY MUST BE DIMENSIONED IN    00007100
C    COMMON EXACTLY AS THE NUMBER OF ROWS AND COLUMNS INDICATE. THE     00007200
C    LINKS ARE SPECIFIED IN TERMS OF DISTANCE FOR THESE PROBLEMS.       00007300
C 9  9 2101                                                             00007400
C$MILLER                                                                00007500
C L= 0.0,4.0,2.0,2.0,0.0,0.0,0.0,0.0,0.0,                               00007600
C    4.0,0.0,1.0,0.0,2.0,0.0,3.0,0.0,0.0,                               00007700
C    2.0,1.0,0.0,4.0,3.0,0.0,0.0,0.0,0.0,                               00007800
C    2.0,0.0,4.0,0.0,0.0,3.0,0.0,0.0,16.0,                              00007900
C    0.0,2.0,3.0,0.0,0.0,3.0,1.0,0.0,0.0,                               00008000
C    0.0,0.0,0.0,3.0,3.0,0.0,0.0,6.0,0.0,                               00008100
C    0.0,3.0,0.0,0.0,1.0,0.0,0.0,6.0,0.0,                               00008200
C    0.0,0.0,0.0,0.0,0.0,6.0,6.0,0.0,5.0,                               00008300
C    0.0,0.0,0.0,16.0,0.0,0.0,0.0,5.0,0.0$                              00008400
C1H ,9F5.1)                                                             00008500
C 9  9 31                                                               00008600
C$MILLER                                                                00008700
C  L(9,8) =13.,                                                         00008800
C  L(3,1) =10.$                                                         00008900
C 7  7 411                                                              00009000
C$MILLER                                                                00009100
C  L(1,2)=0.$                                                           00009200
C03                                                                     00009300
C05                                                                     00009400
C    THE FOLLOWING IS A PARTIAL SAMPLE OF OUTPUT                        00009500
C                                                                       00009600
C MINIMUM PATHS FOR PROBLEM 2 FOR NODE  1                               00009700
C FROM NODE  1 TO NODE 9   16.00                                        00009800
C                                                                       00009900
C MINIMUM ROUTES FROM NODE   1                                          00010000
C *** ROUTE TO NODE  9                                                  00010100
C NODE 8 TO NODE 9    5.00                                              00010200
C NODE 6 TO NODE 8    6.00                                              00010300
C NODE 4 TO NODE 6    3.00                                              00010400
C NODE 1 TO NODE 4    2.00                                              00010500
C                                                                       00010600
C     MINIMUM PATH ALGORITHM                                            00010700
      COMMON L(9,9),X1(9)                                               00010800
      REAL L                                                            00010900
      NAMELIST /MILLER/ L                                               00011000
    5 READ (5,100,END=1000) IROW,ICOL,PROB,KSHORT,INODE,INPRNT          00011100
  100 FORMAT (I3,I3,I2,I1,I1,I1)                                        00011200
C     READ CONTROL CARD  IROW = NUMBER OF ROWS                          00011300
C                        ICOL = NUMBER OF COLS                          00011400
C                        PROB = PROBLEM NUMBER                          00011500
C                        KSHORT = SWITCH  FOR MINIMUM PATH STEP 0=OFF   00011600
C                        INODE =SWITCH FOR INDIVIDUAL NODE PROCESSING   00011700
C     READ LINK ARRAY IN NAMELIST FORM                                  00011800
      READ (5,MILLER)                                                   00011900
      CALL MINPTH (IROW,ICOL,PROB,KSHORT,INODE,INPRNT)                  00012000
      GO TO 5                                                           00012100
 1000 STOP                                                              00012200
      END                                                               00012300
      SUBROUTINE MINPTH (IROW,ICOL,PROB,KSHORT,INODE,INPRNT)            00012400
      COMMON L(9,9),X1(9)                                               00012500
      DIMENSION A(10)                                                   00012600
      REAL L                                                            00012700
      IF (INPRNT .EQ. 0) GO TO 5                                        00012800
      WRITE (6,900) PROB                                                00012900
  900 FORMAT(1H1,55X,20HMINIMUM PATH PROBLEM,2X,I2)                     00013000
      WRITE (6,901)IROW,ICOL                                            00013100
      READ  (5,902)(A(I),I=1,10)                                        00013200
  901 FORMAT(1H0,10HLINK ARRAY,2X,I3,6H ROWS ,I3,5H COLS)               00013300
  902 FORMAT (10A6)                                                     00013400
      WRITE (6,A) ((L(I,J),J=1,ICOL),I=1,IROW)                          00013500
    5 K=0                                                               00013600
   10 DO 6 I=1,IROW                                                     00013700
      X1(I)= 0                                                          00013800
    6 CONTINUE                                                          00013900
C     K= NODE TO WHICH PATHS WILL BE COMPUTED                           00014000
      IF (INODE .EQ. 0) GO TO 7                                         00014100
      READ (5,920,END=1000) K                                           00014200
  920 FORMAT (I3)                                                       00014300
      IF (K .EQ. 0) RETURN                                              00014400
      GO TO 8                                                           00014500
    7 K=K+1                                                             00014600
      IF(K .LE. IROW) GO TO 8                                           00014700
      RETURN                                                            00014800
    8 DO 20 J=1,ICOL                                                    00014900
      IF(L(K,J) .EQ. 0.0) GO TO 20                                      00015000
      X1(J)=L(K,J)                                                      00015100
   20 CONTINUE                                                          00015200
C     COMPUTE FIRST ESTIMATE OF DISTANCES FROM K TO ALL NODES           00015300
   21 DO 30 J=1,ICOL                                                    00015400
      XTEMP=0.0                                                         00015500
      IF(J .EQ.K) GO TO 30                                              00015600
      IF (L(K,J) .GT. 0.0) GO TO 30                                     00015700
      IF(X1(J) .GT. 0.0) GO TO 30                                       00015800
      DO 29 I=1,IROW                                                    00015900
      IF (L(I,J) .EQ. 0.0) GO TO 29                                     00016000
      IF(X1(I) .EQ. 0.0) GO TO 29                                       00016100
      X1(J)=X1(I) + L(I,J)                                              00016200
      IF (XTEMP .EQ. 0.0) XTEMP = X1(J)                                 00016300
      IF (X1(J) .LT. XTEMP) XTEMP =X1(J)                                00016400
   29 CONTINUE                                                          00016500
      X1(J) =XTEMP                                                      00016600
   30 CONTINUE                                                          00016700
      DO 35 I=1,IROW                                                    00016800
      IF(I .EQ. K) GO TO 35                                             00016900
      IF(X1(I) .EQ. 0.0) GO TO 21                                       00017000
   35 CONTINUE                                                          00017100
C     COMPUTE FINAL MINIMUM DISTANCE FROM K TO ALL NODES                00017200
   37 ICOUNT =0                                                         00017300
      DO 40 J=1,ICOL                                                    00017400
      DO 40 I=1,IROW                                                    00017500
      IF(L(I,J) .EQ. 0.0) GO TO 40                                      00017600
      IF((X1(J)-X1(I) -L(I,J)).LT. .0001) GO TO 40                      00017700
      ICOUNT =ICOUNT +1                                                 00017800
      X1(J)=L(I,J) +X1(I)                                               00017900
   40 CONTINUE                                                          00018000
      IF (ICOUNT .NE. 0) GO TO 37                                       00018100
      WRITE (6,903) PROB,K                                              00018200
  903 FORMAT(1H1,26HMINIMUM PATHS FOR PROBLEM ,    I2,10H FOR NODE ,I3) 00018300
  904 FORMAT(1H ,10HFROM NODE ,I3,1X,8HTO NODE ,I3,5X,F10.2)            00018400
      DO 41 I=1,IROW                                                    00018500
      WRITE (6,904)K,I,X1(I)                                            00018600
   41 CONTINUE                                                          00018700
      IF(KSHORT .EQ. 0  ) GO TO 51                                      00018800
C     FIND MINIMUM ROUTE FROM NODE TO NODE                              00018900
      WRITE(6,905) K                                                    00019000
 905  FORMAT (1H1,37HMINIMUM NODE TO NODE ROUTES FROM NODE,             00019100
     12X,I3)                                                            00019200
      DO 50 J=1,ICOL                                                    00019300
      IF (J .EQ. K) GO TO 50                                            00019400
      MM =0                                                             00019500
      WRITE(6,907) J                                                    00019600
  907 FORMAT(1H0,17H*** ROUTE TO NODE,1X,I3)                            00019700
      M=J                                                               00019800
   44 DO 45 I=1,IROW                                                    00019900
      IF(MM .EQ. I) GO TO 45                                            00020000
      IF (L(I,M) .EQ. 0.0) GO TO 45                                     00020100
      IF ((X1(M) -X1(I)) .LT. 0.0 )GO TO 45                             00020200
      IF((X1(M)-X1(I) -L(I,M)) .LT. 0.0) GO TO 45                       00020300
      IF((X1(M)-X1(I) -L(I,M)) .LT. .0001)   GO TO 46                   00020400
   45 CONTINUE                                                          00020500
      STOP                                                              00020600
   46 WRITE (6,906)I,M,L(I,M)                                           00020700
  906 FORMAT(1H ,5HNODE ,I3,1X,8HTO NODE ,I3,3X,F7.2)                   00020800
      MM=M                                                              00020900
      M=I                                                               00021000
      IF (M .EQ. K) GO TO 50                                            00021100
      GO TO 44                                                          00021200
   50 CONTINUE                                                          00021300
   51 CONTINUE                                                          00021400
      GO TO 10                                                          00021500
 1000 STOP                                                              00021600
      END                                                               00021700
