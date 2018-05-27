C   1  84      SUBROUTINE AARGS                    2 19 68              00000000
FILE  1 = OMNITAB,UNIT=READER                                           00000100
FILE  3 = OMNITAB,UNIT=PRINTER                                          00000200
FILE  2 = OMNITAB,UNIT=PUNCH                                            00000300
FILE  4 = OMNITAB,UNIT=DISK,AREA=6030,BLOCKING=30,RECORD=11,BUFFER=2    00000400
C          THE PROGRAM WAS DEVELOPED AT THE NATIONAL BUREAU OF STANDARDS00000500
C     IN WASHINGTON D.C.   MOST OF ITS IDEAS AND PHILOSOPHIES ARE DUE   00000600
C     TO J. HILSENRATH AND ARE EXPLAINED IN "OMNITAB", A COMPUTER       00000700
C     PROGRAM FOR STATISTICAL AND NUMERICAL ANALYSIS, NATIONAL BUREAU   00000800
C     OF STANDARD HANDBOOK 101 (1968), A MANUAL WRITTEN TO DESCRIBE THE 00000900
C     ORIGINAL VERSION.    THIS HANDBOOK MAY BE PROCURED FROM THE       00001000
C     GOVERNMENT PRINTING OFFICE, WASHINGTON, D. C.                     00001100
C          THE ORIGINAL VERSION WAS WRITTEN PRIMARILY IN ASSEMBLY       00001200
C     LANGUAGE FOR THE IBM 7094 AND WAS THEREFORE ALMOST EXCLUSIVELY    00001300
C     AVAILABLE ONLY TO THOSE PERSONS WHO HAD ACCESS TO SUCH A MACHINE. 00001400
C     BECAUSE OMNITAB PROVED TO BE SUCH A SUCCESS IN PROVIDING ACCESS TO00001500
C     THE COMPUTER FOR PEOPLE WHO KNOW ALMOST NOTHING ABOUT COMPUTER    00001600
C     PROGRAMMING, THE PROGRAM WAS REWITTEN DURING THE PAST YEAR IN     00001700
C     FORTRAN IN AN ATTEMPT TO MAKE THE PROGRAM AVAILABLE TO AS MANY    00001800
C     INSTALLATIONS AS POSSIBLE.                                        00001900
C          THE VERSION OF THE PROGRAM DESCRIBED HERE WAS WRITTEN FOR    00002000
C     THE NATIONAL BUREAU OF STANDARDS" UNIVAC 1108 INSTALLATION BY     00002100
C     WALTER J. GILBERT, PHILIP WALSH, CARLA MESSINA, SALLY PEAVY, AND  00002200
C     RUTH VARNER WITH THE COOPERATION OF THEIR STATISTICAL ENGINEERING 00002300
C     LABORATORY.   IT WAS ADAPTED FOR USE ON THE IBM 360/65 OF IOWA    00002400
C     STATE UNIVERSITY BY R. L. CHAMBERLAIN UNDER PROJECT 101 OF THE    00002500
C     IOWA AGRICULTURAL EXPERIMENT STATION, WHERE IT HAS PROVED         00002600
C     INVALUABLE IN STATISTICAL CONSULTING.  IT WAS CONVERTED FOR USE   00002700
C     ON THE B5500 OF THE NAVAL AIR TEST CENTER BY SYSTEMS DESIGN AND   00002800
C     PROGRAMMING SECTION OF COMPUTER SERVICES AND OPERATIONS BRANCH    00002900
C     OF COMPUTER SERVICES DIVISION NAVAL AIR TEST CENTER PATUXENT      00003000
C     RIVER, MARYLAND                                                   00003100
      SUBROUTINE AARGS                                                  00003200
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00003300
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00003400
C                                                                       00003500
C     THIS SUBROUTINE ASSEMBLES A FLOATING POINT NUMBER FROM A STRING OF00003600
C     DIGITS ETC.  M INITIALLY POINTS AT THE FIRST NUMBER. IT IS LEFT   00003700
C     POINTING AT THE FIRST CHARACTER AFTER THE NUMBER.                 00003800
C                                                                       00003900
C     VALUE RETURNED IN ARG                                             00004000
C                                                                       00004100
C     KARG = 1 = FLOATING POINT, = 0 = INTEGER, -1 = ERROR.             00004200
C                                                                       00004300
      ARG=KARD(M)                                                       00004400
      SIG =1.                                                           00004500
      JEXP=0                                                            00004600
      IXS=1                                                             00004700
      IEXP=0                                                            00004800
      KARG=0                                                            00004900
C                                                                       00005000
C     LOOK BACK FOR MINUS SIGN AND/OR DECIMAL POINT                     00005100
C                                                                       00005200
      K=KARD(M-1)                                                       00005300
      IF(K.NE.37)GO TO 10                                               00005400
      KARG=1                                                            00005500
      IEXP=-1                                                           00005600
      K=KARD(M-2)                                                       00005700
  10  IF(K.EQ.38)SIG =-1.                                               00005800
  20  M=M+1                                                             00005900
      K=KARD(M)                                                         00006000
      IF(K.GE.10)GO TO 30                                               00006100
      IEXP=IEXP-KARG                                                    00006200
      ARG=10.*ARG+FLOAT(K)                                              00006300
      GO TO 20                                                          00006400
  30  IF(K.NE.37)GO TO 50                                               00006500
C                                                                       00006600
C     DECIMAL POINT FOUND                                               00006700
C                                                                       00006800
      IF(KARG.EQ.0)GO TO 40                                             00006900
      CALL ERROR(3)                                                     00007000
      KARG=-1                                                           00007100
      RETURN                                                            00007200
  40  KARG=1                                                            00007300
      GO TO 20                                                          00007400
C                                                                       00007500
C     CHECK FOR EXPONENT   E X, E+X, E-X, +X, -X                        00007600
C                                                                       00007700
  50  IF( K .NE. 14 ) GO TO 54                                          00007800
      M = M + 1                                                         00007900
      K = KARD( M )                                                     00008000
      IF( K .NE. 44 ) IF( K - 10 ) 56, 54, 54                           00008100
  52  M = M + 1                                                         00008200
      K = KARD( M )                                                     00008300
      IF( K - 10 ) 56, 100, 100                                         00008400
  54  IF( K .NE. 38 ) IF( K - 39 ) 100, 52, 100                         00008500
      IXS = -1                                                          00008600
      GO TO 52                                                          00008700
  56  KARG = KARG + 1                                                   00008800
  70  JEXP=10*JEXP+K                                                    00008900
      M=M+1                                                             00009000
      K=KARD(M)                                                         00009100
      IF( K .LT. 10 ) GO TO 70                                          00009200
C                                                                       00009300
C     DONE WITH ARGUMENT                                                00009400
C                                                                       00009500
 100  IF(KARG.NE.0)GO TO 120                                            00009600
 110  ARG=SIG *ARG                                                      00009700
      RETURN                                                            00009800
 120  KARG=1                                                            00009900
      IEXP = IXS * JEXP + IEXP                                          00010000
C                                                                       00010100
C     THE FOLLOWING CODING YIELDS MORE ACCURATE RESULTS THAN THE        00010200
C     OBVIOUS    ARG = ARG * 10. * IEXP                                 00010300
C                                                                       00010400
      JEXP = IABS( IEXP )                                               00010500
      IF( JEXP .GT. IFIX( XALOG ) ) GO TO 130                           00010600
      IF( IEXP ) 123, 110, 126                                          00010700
 123  ARG = ARG / 10. ** JEXP                                           00010800
      GO TO 110                                                         00010900
 126  ARG = ARG * 10. ** JEXP                                           00011000
      GO TO 110                                                         00011100
 130  CALL ERROR( 102 )                                                 00011200
      ARG = 0.                                                          00011300
      GO TO 110                                                         00011400
      END                                                               00011500
C   2  21      SUBROUTINE ADRESS( I, J )           2 19 68              00011600
      SUBROUTINE ADRESS( I, J )                                         00011700
      COMMON / BLOCKF / NCTOP                                           00011800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00011900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00012000
      DIMENSION ARGS(100)                                               00012100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00012200
C                                                                       00012300
C         CALCULATE ADDRESS OF ARGUMENT( I ). IF ARGUMENT( I ) IS A     00012400
C         FLOATING POINT NUMBER, J = -(I+5000). IF ILLEGAL COLUMN NUMBER00012500
C         J = 0.  IF OK, J = ADDRESS                                    00012600
C                                                                       00012700
      IF( KIND( I ) .EQ. 0 ) GO TO 10                                   00012800
C     THE 10000 IS THE SIZE OF THE ARRAY                                00012900
      J = -( I + 10000)                                                 00013000
      GO TO 30                                                          00013100
  10  IF( IARGS( I ) .GE. 1 .AND. IARGS( I ) . LE. NCOL ) GO TO 20      00013200
      J = 0                                                             00013300
      GO TO 30                                                          00013400
   20 J = ( NROW+NCTOP-1 ) * ( IARGS(I)-1 ) + NCTOP                     00013500
  30  RETURN                                                            00013600
      END                                                               00013700
C   3 155      SUBROUTINE ALLSUB                   2 19 68              00013800
      SUBROUTINE ALLSUB                                                 00013900
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00014000
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00014100
      DIMENSION ARGS(100)                                               00014200
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00014300
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00014400
      COMMON / SCRAT / SCRA(10000),NS                                   00014500
      EQUIVALENCE(L11,LL1),(L22,LL2)                                    00014600
C     PROGRAMMED BY PHILIP J. WALSH (NBS 453.40) MAY, 1967              00014700
C                                                                       00014800
C                                                                       00014900
C     COMMAND IS OF THE FORM XXXX OF ORDER ++ OF COL ++, STORE IN ++    00015000
C     XXXX MAY BE  (A) NLSUB FOR NORMALIZED LAGUERRE POLYNOMIALS        00015100
C                  (B) LSUB  FOR LAGUERRE POLYNOMIALS                   00015200
C                  (C) HSUB  FOR HERMITE POLYNOMIALS                    00015300
C                  (D) USUB  FOR CHEBYSHEV POLYNOMIALS                  00015400
C                  (E) PSUB  FOR LEGENDRE  POLYNOMIALS                  00015500
C                  (F) TSUB  FOR CHEBYSHEV POLYNOMIALS                  00015600
C    SEE RECURSIVE FORMULAE FOR THESE POLYNOMIALS FURTHER IN CODE       00015700
C     EACH OF THE COMMANDS REQUIRE THREE ARGUMENTS                      00015800
      IF ( NARGS .EQ. 3 ) GO TO 1                                       00015900
  200 CALL ERROR(10)                                                    00016000
      GO TO 99                                                          00016100
   1  IF( KIND( 1 ) + KIND( 3 ) .EQ. 0 ) GO TO 2                        00016200
  13  CALL ERROR( 3 )                                                   00016300
      GO TO 99                                                          00016400
C CHECK THAT X IS WITHIN WORKSHEET AND GET ADDRESS OF ARGUMENT COLUMN   00016500
    2 CALL ADRESS( 2, L11 )                                             00016600
      IF( L11 ) 13, 25, 3                                               00016700
  25  CALL ERROR( 11 )                                                  00016800
      GO TO 99                                                          00016900
   3  IARGS( 4 ) = IARGS( 1 ) + IARGS( 3 ) - 1                          00017000
      KIND( 4 ) = 0                                                     00017100
      CALL ADRESS( 4, L22 )                                             00017200
      IF( L22 .LE. 0 ) GO TO 25                                         00017300
   4  CALL ADRESS( 3, L22 )                                             00017400
      IF (NRMAX .NE. 0 ) GO TO 6                                        00017500
      CALL ERROR(9)                                                     00017600
      GO TO 99                                                          00017700
   6  IF( NERROR .NE. 0 ) GO TO 99                                      00017800
      IJK = LL1                                                         00017900
      IJ = LL2                                                          00018000
      DO 12 I = 1, NRMAX                                                00018100
      SCRA( 1 ) = RC( IJK )                                             00018200
      GO TO ( 8,8,9,9,10,10) , L2                                       00018300
   8  RC( IJ ) = 1. - SCRA( 1 )                                         00018400
      GO TO 11                                                          00018500
   9  RC( IJ ) = 2. * SCRA( 1 )                                         00018600
      GO TO 11                                                          00018700
  10  RC( IJ ) = SCRA( 1 )                                              00018800
  11  IJK = IJK + 1                                                     00018900
  12  IJ = IJ + 1                                                       00019000
      IF(IARGS(1) .EQ. 1 ) GO TO 99                                     00019100
      N = IARGS(1)-1                                                    00019200
      DO 101 J = 1,NRMAX                                                00019300
      IJK = LL1 + J                                                     00019400
      IJ = LL2 + J                                                      00019500
      SCRA (1) = 1.0                                                    00019600
      SCRA( 2 ) = RC( IJK - 1 )                                         00019700
      SCRA( 3 ) = RC( IJ - 1 )                                          00019800
      SCRA (4) = 1.0                                                    00019900
      SCRA (5) = 2.0                                                    00020000
      DO 100 I = 1,N                                                    00020100
      IARGS( 4 ) = IARGS( 3 ) + I                                       00020200
      CALL ADRESS( 4, LL22 )                                            00020300
      GO TO(30,40,50,60,70,80) , L2                                     00020400
C   ITYPE = 1      NLSUB          NORMALIZED LAGUERRE POLYNOMIALS       00020500
C             RECURSION FORMULA  L(N+1) =(1.+2.*N-X)*L(N)-N**2 *L(N-1)  00020600
C             L(0) = 1.                                                 00020700
C             L(1) = -X+1.                                              00020800
C             L(2) = X**2 - 4.0*X +2.                                   00020900
C             L(3) =-X**3 + 9.0*X**2-18.0*X+6.                          00021000
C                                                                       00021100
C     L(N)= EXP(X)*(DN/DXN(X**N*EXP(-X)))                               00021200
C                                                                       00021300
   30 SCRA (4) = I                                                      00021400
      SCRA (6) = 1.0 + 2.0*SCRA (4)                                     00021500
      SCRA (7) = SCRA (4)*SCRA (4)                                      00021600
      SCRA (8) = (SCRA (6)-SCRA (2))*SCRA (3)-SCRA (7)*SCRA (1)         00021700
      GO TO 90                                                          00021800
C   ITYPE = 2      LSUB           LAGUERRE POLYNOMIALS                  00021900
C             RECURSION FORMULA  L(N+1)=(((2.*N+1)-X)*L(N)-N*L(N-1))/   00022000
C                                        (N+1)                          00022100
C             L(0) =  1.                                                00022200
C             L(1) =  -X+1.                                             00022300
C             L(2) =  .5 (XX*2 - 4.*X +2)                               00022400
C             L(3) =  (-X**3 + 9.*X**2 - 18.* X +6.)/6.                 00022500
C                                                                       00022600
C  **** SEE  ABRAMOWITZ, M. AND STEGUN, I.A.,  HANDBOOK OF MATHEMATICAL 00022700
C            FUNCTIONS, NATIONAL BUREAU OF STANDARDS APPLIED MATHEMATICS00022800
C            SERIES 55, SUPERINTENDENT OF DOCUMENTS, U.S. GOVERNMENT    00022900
C            PRINTING OFFICE, WASHINGTON, D.C. 20402                    00023000
C                                                                       00023100
C  **** SEE  HILSENRATH,ZIEGLER,MESSINA,WALSH,HERBOLD,, OMNITAB, NBS    00023200
C            HANDBOOK 101 (MARCH 4, 1966) -  FOR FORMULAE USED          00023300
   40 SCRA (4) = I                                                      00023400
      SCRA (6) = SCRA (4) + 1.0                                         00023500
      SCRA (7) = SCRA (4) + SCRA (6)                                    00023600
      SCRA (8) = ((SCRA (7)-SCRA (2))*SCRA (3)-SCRA (4)*SCRA (1))/      00023700
     1           SCRA (6)                                               00023800
      GO TO 90                                                          00023900
C   ITYPE = 3      HSUB           HERMITE POLYNOMIALS                   00024000
C             RECURSION FORMULA  H(N+1) = 2.0*X*H(N)-2.0*N*H(N-1)       00024100
C                                                                       00024200
C             H(0) = 1.                                                 00024300
C             H(1) = 2.0*X                                              00024400
C             H(2) = 4.0*X**2-2.                                        00024500
C             H(3) = 8.0*X**3-12.*X                                     00024600
   50 SCRA (8)=2.0*(SCRA (2)*SCRA (3)-SCRA (4)*SCRA (1))                00024700
      SCRA (4) = SCRA (4) + 1.0                                         00024800
      GO TO 90                                                          00024900
C   ITYPE = 4      USUB           CHEBYSHEV POLYNOMIALS                 00025000
C                                                                       00025100
C             RECURSION FORMULA  U(N) = 2.0*X*U(N-1)-U(N-2)             00025200
C                                                                       00025300
C             U(0) = 1.                                                 00025400
C             U(1) = 2.0*X                                              00025500
C             U(2) = 4.0*X**2-1.0                                       00025600
C             U(3) = 8.0*X**3-4.0*X                                     00025700
C                                                                       00025800
   60 SCRA (8) = 2.0*SCRA (2)*SCRA (3)-SCRA (1)                         00025900
      GO TO 90                                                          00026000
C   ITYPE = 5      PSUB           LEGENDRE POLYNOMIALS                  00026100
C                                                                       00026200
C             RECUSION FORMULA  P(N+1) =X*P(N)+(N/N+1)*(X*P(N)-P(N-1))  00026300
C                                                                       00026400
C             P(0) = 1.                                                 00026500
C             P(1) = X.                                                 00026600
C             P(2) = (3./2.)*X**2-(1./2.)                               00026700
C             P(3) = 2.5*X**3-1.5*X                                     00026800
C                                                                       00026900
   70 SCRA (6)=SCRA (4)/SCRA (5)                                        00027000
      SCRA (8)=(1.0+SCRA (6))*SCRA (2)*SCRA (3)-SCRA (6)*SCRA (1)       00027100
      SCRA (4) = SCRA (5)                                               00027200
      SCRA (5) = SCRA (5) + 1.0                                         00027300
      GO TO 90                                                          00027400
C   ITYPE = 6      TSUB           CHEBYSHEV POLYNOMIALS                 00027500
C                                                                       00027600
C             RECURSION FORMULA                                         00027700
C                                                                       00027800
C             T(0) = 1.                                                 00027900
C             T(1) = X                                                  00028000
C             T(2) = 2.*X**2-1.                                         00028100
C             T(3) = 4.*X**3-3.*X                                       00028200
   80 SCRA (8) = 2.0 * SCRA (2)*SCRA (3)-SCRA (1)                       00028300
   90 CONTINUE                                                          00028400
  91  LJMN = LL22 + J                                                   00028500
      RC( LJMN - 1 ) = SCRA( 8 )                                        00028600
      SCRA (1) = SCRA (3)                                               00028700
      SCRA (3) = SCRA (8)                                               00028800
   98 CONTINUE                                                          00028900
  100 CONTINUE                                                          00029000
  101 CONTINUE                                                          00029100
   99 RETURN                                                            00029200
      END                                                               00029300
C   4  35      SUBROUTINE APRINT                   2 19 68              00029400
      SUBROUTINE APRINT                                                 00029500
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00029600
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00029700
      DIMENSION ARGS(100)                                               00029800
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00029900
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00030000
      IF( NARGS .EQ. 4 ) GO TO 30                                       00030100
  10  CALL ERROR( 205 )                                                 00030200
  20  RETURN                                                            00030300
  30  J = 1                                                             00030400
      I = 4                                                             00030500
      CALL CKIND( I )                                                   00030600
      IF( I .NE. 0 ) GO TO 10                                           00030700
      K = IARGS( 1 )                                                    00030800
      CALL MTXCHK( J )                                                  00030900
      IF( J .NE. 0 .OR. IARGS( 4 ) .GE. 50 ) GO TO 10                   00031000
      IARGS( 1 ) = K                                                    00031100
      K = IARGS( 3 )                                                    00031200
      IARGS( 51 ) = IARGS( 1 )                                          00031300
      IARGS( 52 ) = IARGS( 2 )                                          00031400
      L = IARGS( 4 )                                                    00031500
      DO 40 I = 2, L                                                    00031600
      IARGS( I+1 ) = IARGS( I ) + 1                                     00031700
      IARGS( I+51 ) = IARGS( I+1 )                                      00031800
  40  KIND( I+1 ) = 0                                                   00031900
      NARGS = L + 1                                                     00032000
      L1 = 6                                                            00032100
      LL = IARGS( 1 )                                                   00032200
  50  DO 60 I = 1, K                                                    00032300
      CALL PRINTX                                                       00032400
      IARGS( 51 ) = LL + I                                              00032500
      DO 60 J = 1, NARGS                                                00032600
  60  IARGS( J ) = IARGS( J+50 )                                        00032700
      GO TO 20                                                          00032800
      END                                                               00032900
C   5  90      SUBROUTINE ARITH                    2 19 68              00033000
      SUBROUTINE ARITH                                                  00033100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00033200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00033300
      DIMENSION ARGS(100)                                               00033400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00033500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00033600
      DIMENSION II( 4 ), KK( 4 )                                        00033700
      EQUIVALENCE (I1,II(1)),(I2,II(2)),(I3,II(3)),(I4,II(4))           00033800
C                                                                       00033900
C         THIS SUBROUTINE PERFORMS ADD, SUB, MULT DIV, RAISE FOR        00034000
C         THREE AND FOUR ARGUMENTS.                                     00034100
C                                                                       00034200
      IF( NARGS .EQ. 3 .OR. NARGS .EQ. 4 ) GO TO 2                      00034300
      CALL ERROR( 10 )                                                  00034400
      GO TO 10                                                          00034500
    2 IF( KIND( NARGS ) .EQ. 0 ) GO TO 15                               00034600
      CALL ERROR ( 20 )                                                 00034700
      GO TO 10                                                          00034800
    5 CALL ERROR( 11 )                                                  00034900
   10 RETURN                                                            00035000
   15 DO 30 I = 1, NARGS                                                00035100
      KK( I ) = 1                                                       00035200
      CALL ADRESS( I, II( I ) )                                         00035300
      IF( II( I ) ) 20, 5, 30                                           00035400
   20 KK( I ) = 0                                                       00035500
      II( I ) = -II( I )                                                00035600
   30 CONTINUE                                                          00035700
      IF( NERROR .NE. 0 ) GO TO 10                                      00035800
      IF( NRMAX .GT. 0 ) GO TO 40                                       00035900
      CALL ERROR( 9 )                                                   00036000
      GO TO 10                                                          00036100
  40  JJ = II( NARGS ) + NRMAX - 1                                      00036200
      IJ = L2 + 5 * ( NARGS - 3 )                                       00036300
      GO TO (100,200,300,400,500,600,700,800,900,1000), IJ              00036400
  100 DO 110 I = I3, JJ                                                 00036500
      RC( I ) = RC( I1 ) + RC( I2 )                                     00036600
      I1 = I1 + KK( 1 )                                                 00036700
  110 I2 = I2 + KK( 2 )                                                 00036800
      GO TO 10                                                          00036900
  200 DO 210 I = I3, JJ                                                 00037000
      RC( I ) = RC( I2 ) - RC( I1 )                                     00037100
      I1 = I1 + KK( 1 )                                                 00037200
  210 I2 = I2 + KK( 2 )                                                 00037300
      GO TO 10                                                          00037400
  300 DO 310 I = I3, JJ                                                 00037500
      RC( I ) = RC( I1 ) * RC( I2 )                                     00037600
      I1 = I1 + KK( 1 )                                                 00037700
  310 I2 = I2 + KK( 2 )                                                 00037800
      GO TO 10                                                          00037900
  400 DO 410 I = I3, JJ                                                 00038000
      RC( I ) = RC( I1 ) / RC( I2 )                                     00038100
      I1 = I1 + KK( 1 )                                                 00038200
  410 I2 = I2 + KK( 2 )                                                 00038300
      GO TO 10                                                          00038400
  500 DO 510 I = I3, JJ                                                 00038500
      RC( I ) = FEXP2( RC( I1 ), RC( I2 ) )                             00038600
      I1 = I1 + KK( 1 )                                                 00038700
  510 I2 = I2 + KK( 2 )                                                 00038800
      GO TO 10                                                          00038900
  600 DO 610 I = I4, JJ                                                 00039000
      RC( I ) = RC( I ) + ( RC( I1 ) + RC( I2 ) ) * RC( I3 )            00039100
      I1 = I1 + KK( 1 )                                                 00039200
      I2 = I2 + KK( 2 )                                                 00039300
  610 I3 = I3 + KK( 3 )                                                 00039400
      GO TO 10                                                          00039500
  700 DO 710 I = I4, JJ                                                 00039600
      RC( I ) = RC( I ) + ( RC( I2 ) - RC( I1 ) ) * RC( I3 )            00039700
      I1 = I1 + KK( 1 )                                                 00039800
      I2 = I2 + KK( 2 )                                                 00039900
  710 I3 = I3 + KK( 3 )                                                 00040000
      GO TO 10                                                          00040100
  800 DO 810 I = I4, JJ                                                 00040200
      RC( I ) = RC( I ) + ( RC( I1 ) * RC( I2 ) ) * RC( I3 )            00040300
      I1 = I1 + KK( 1 )                                                 00040400
      I2 = I2 + KK( 2 )                                                 00040500
  810 I3 = I3 + KK( 3 )                                                 00040600
      GO TO 10                                                          00040700
  900 DO 910 I = I4, JJ                                                 00040800
      RC( I ) = RC( I ) + ( RC( I1 ) / RC( I2 ) ) * RC( I3 )            00040900
      I1 = I1 + KK( 1 )                                                 00041000
      I2 = I2 + KK( 2 )                                                 00041100
  910 I3 = I3 + KK( 3 )                                                 00041200
      GO TO 10                                                          00041300
 1000 DO 1010 I = I4, JJ                                                00041400
      RC( I ) = RC( I ) + RC( I3 ) * FEXP2( RC( I1 ), RC( I2 ) )        00041500
      I1 = I1 + KK( 1 )                                                 00041600
      I2 = I2 + KK( 2 )                                                 00041700
 1010 I3 = I3 + KK( 3 )                                                 00041800
      GO TO 10                                                          00041900
      END                                                               00042000
C   6 134      SUBROUTINE ARYVEC                   2 19 68              00042100
      SUBROUTINE ARYVEC                                                 00042200
C     SUBROUTINE ARYVEC              9/27/67                            00042300
C *****                                                                 00042400
C     SUBROUTINE TO MULTIPLY MATRIX TIME VECTOR                         00042500
C                        OR VECTOR TRANSPOSE TIME MATRIX                00042600
C     L2=1 MULTIPLY MATRIX TIME VECTOR                                  00042700
C          GENERAL FORM OF COMMAND                                      00042800
C              M(AV)  A (,) N,K   VECTOR IN COL I  STORE IN COLUMN J    00042900
C              M(AV)  A (,) N,K   VECTOR IN COL I  STORE IN ROW K COL J 00043000
C                   N AND K MUST BE SPECIFIED                           00043100
C     L2=2 MULTIPLY VECTOR TRANSPOSE TIMES MATRIX                       00043200
C          GENERAL FORM OF COMMAND                                      00043300
C              M(VTA) A (,) N,K  VECTOR IN COL I  STORE IN ROW  J       00043400
C              M(VTA) A (,) N,K  VECTOR IN COL I  STORE IN ROW K COL J  00043500
C                   N AND K MUST BE SPECIFIED                           00043600
C                   IF ONLY ROW IS GIVEN FOR STORAGE  COL  1 IS ASSUMED 00043700
C *****                                                                 00043800
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00043900
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00044000
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00044100
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00044200
      DIMENSION ARGS(100)                                               00044300
      EQUIVALENCE( ARGS(1), RC(5001) )                                  00044400
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00044500
      COMMON / SCRAT / X , NS                                           00044600
      DIMENSION A( 10000 )                                              00044700
      DOUBLE PRECISION X(5000), SUM                                     00044800
      COMMON /MULTC/ NS2                                                00044900
      NS2 = NS/2                                                        00045000
C *****                                                                 00045100
C     CHECK FOR CORRECT NUMBER OF ARGUMENTS                             00045200
C *****                                                                 00045300
      IF(NARGS.NE.6.AND.NARGS.NE.7) CALL ERROR(10)                      00045400
C *****                                                                 00045500
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS                        00045600
C *****                                                                 00045700
      J=NARGS                                                           00045800
      CALL CKIND(J)                                                     00045900
      IF(J.NE.0) CALL ERROR(3)                                          00046000
C *****                                                                 00046100
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE                       00046200
C *****                                                                 00046300
      GO TO (200,220),L2                                                00046400
 200  IADD=IARGS(4)                                                     00046500
      IADD2=IARGS(3)                                                    00046600
      ICOMP=NROW                                                        00046700
      GO TO 240                                                         00046800
 220  IADD=IARGS(3)                                                     00046900
      IADD2=IADD                                                        00047000
      ICOMP=NCOL                                                        00047100
C *****                                                                 00047200
C     COMPUTE ADDRESSES OF COLUMNS                                      00047300
C *****                                                                 00047400
  240 IARGS(10)=IARGS(NARGS)                                            00047500
      IARGS(8)=1                                                        00047600
      GO TO ( 440 , 410 ),L2                                            00047700
 410  IF(NARGS.EQ.7) GO TO 420                                          00047800
      J=2                                                               00047900
      IROWSV=IARGS(6)                                                   00048000
      GO TO 430                                                         00048100
 420  IARGS(12)=IARGS(4)                                                00048200
      IARGS(11)=1                                                       00048300
      IARGS(9)=IARGS(6)                                                 00048400
      J=3                                                               00048500
 430  IARGS(7)=IARGS(3)                                                 00048600
      GO TO 460                                                         00048700
 440  J=3                                                               00048800
      IARGS(12)=1                                                       00048900
      IARGS(11)=IARGS(3)                                                00049000
      IARGS(7)=IARGS(4)                                                 00049100
      IF(NARGS.EQ.6) GO TO 450                                          00049200
      IARGS(9)=IARGS(6)                                                 00049300
      GO TO 460                                                         00049400
 450  IARGS(9)=1                                                        00049500
 460  IARGS(6)=IARGS(5)                                                 00049600
      IARGS(5)=1                                                        00049700
      CALL MTXCHK(J)                                                    00049800
      IF(J-1) 490,470,480                                               00049900
  470 CALL ERROR(3)                                                     00050000
      RETURN                                                            00050100
  480 CALL ERROR (17)                                                   00050200
      RETURN                                                            00050300
C *****                                                                 00050400
C     CHECK FOR PREVIOUS ERRORS                                         00050500
C *****                                                                 00050600
  490 IF(NERROR.NE.0) RETURN                                            00050700
      GO TO (580 ,600  ),L2                                             00050800
 580  ICS=IARGS(9)                                                      00050900
      IAP=IARGS(1)                                                      00051000
      IP=IARGS(3)                                                       00051100
      JP=IARGS(4)                                                       00051200
      IAD1=NROW                                                         00051300
      IAD2=1                                                            00051400
      IBP=IARGS(5)                                                      00051500
      GO TO 660                                                         00051600
 600  IBP=IARGS(1)                                                      00051700
      IAP=IARGS(5)                                                      00051800
      IP=IARGS(4)                                                       00051900
      IF(NARGS.EQ.7) GO TO 620                                          00052000
      JP=IARGS(3)                                                       00052100
      ICS=IROWSV                                                        00052200
      GO TO 640                                                         00052300
 620  JP=IARGS(3)                                                       00052400
      ICS=IARGS(9)                                                      00052500
 640  IAD1=1                                                            00052600
      IAD2=NROW                                                         00052700
 660  IC=1                                                              00052800
      DO 740  I=1,IP                                                    00052900
      IA=IAP                                                            00053000
      IB=IBP                                                            00053100
      IS=NS2                                                            00053200
      DO 680  J=1,JP                                                    00053300
      X(IS)=RC(IA)*RC(IB)                                               00053400
      IS=IS-1                                                           00053500
      IA=IA+IAD1                                                        00053600
 680  IB=IB+1                                                           00053700
      CALL SORTSM(JP,SUM)                                               00053800
      A(IC) = SUM                                                       00053900
      IC=IC+1                                                           00054000
      GO TO (700,720),L2                                                00054100
 700  IAP=IAP+1                                                         00054200
      GO TO 740                                                         00054300
 720  IBP=IBP+NROW                                                      00054400
 740  CONTINUE                                                          00054500
C *****                                                                 00054600
C     STORE RESULTS IN WORKSHEET                                        00054700
C *****                                                                 00054800
      IS=1                                                              00054900
      DO  800    I=1,IP                                                 00055000
      RC(ICS) = A(IS)                                                   00055100
      IS=IS+1                                                           00055200
      ICS=ICS+IAD2                                                      00055300
 800  CONTINUE                                                          00055400
      RETURN                                                            00055500
      END                                                               00055600
C   7  88      SUBROUTINE ASTER                    2 19 68              00055700
      SUBROUTINE ASTER                                                  00055800
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00055900
      DIMENSION NAM(2)                                                  00056000
C                                                                       00056100
C     ASTERISKS HAVE BEEN FOUND, LOOK FOR A SPECIAL FORM OF ARGUMENT.   00056200
C                                                                       00056300
C     FORMS CAN BE..                                                    00056400
C                                                                       00056500
C     *PHYSCON*    A PHYSICAL CONSTANT NAME, FL.PT.                     00056600
C     **VARCON**   A -VARIABLE- CONSTANT TO BE USED AS AN INTEGER (TRUN)00056700
C     *VARCON*     A -VARIABLE- CONSTANT TO BE USED AS A FL.PT. NUMBER  00056800
C     **ROW,COLUMN**   A WORKSHEET ENTRY TO BE TRUNCATED AND USED AS INT00056900
C     *ROW,COLUMN*     A WORKSHEET ENTRY TO BE USED AS FLOATING POINT   00057000
C                                                                       00057100
C     NONBLA IS A FUNCTION WHICH RETURNS THE NEXT NON-BLANK CHARACTER   00057200
C     IN THE CARD AND ALSO POINTS M AT IT                               00057300
C                                                                       00057400
C        KARG = 1, SINGLE *.  KARG = 0, DOUBLE *.                       00057500
C                                                                       00057600
  155 CONTINUE                                                          00057700
      I=M                                                               00057800
      L=KARG                                                            00057900
      K=NONBLA(I)                                                       00058000
  10  IF(K.NE.40)GO TO 20                                               00058100
C                                                                       00058200
C     A LONG LINE OF ASTERISKS FOUND, SKIP OVER THEM AND IGNORE         00058300
C                                                                       00058400
      KARG=7                                                            00058500
  15  M=M+1                                                             00058600
      IF(KARD(M)-40)120,15,120                                          00058700
  20  IF(K.GE.36)GO TO 999                                              00058800
      IF(K.GE.10)GO TO 50                                               00058900
C                                                                       00059000
C     NUMBER IS FIRST NON-BLANK CHARACTER, SET N = COMMA                00059100
C                                                                       00059200
      N=43                                                              00059300
  30  CALL AARGS                                                        00059400
      IF(KARG.NE.0) GO TO 999                                           00059500
      I = M                                                             00059600
      IF(NONBLA(I).EQ.N)IF(N-40)40,45,40                                00059700
      GO TO 999                                                         00059800
   40 I = M + 1                                                         00059900
      IF(NONBLA( I ).GE.10)GO TO 999                                    00060000
C                                                                       00060100
C     SET N = ASTERISK                                                  00060200
C                                                                       00060300
      N=40                                                              00060400
      T=ARG                                                             00060500
      GO TO 30                                                          00060600
  45  ARG2=ARG                                                          00060700
      ARG=T                                                             00060800
      KARG=5                                                            00060900
      GO TO 100                                                         00061000
C                                                                       00061100
C     LETTER FOUND FIRST                                                00061200
C                                                                       00061300
  50  CALL NNAME(NAM(1))                                                00061400
      CALL PHYCON(NAM(1))                                               00061500
      IF(ARG.EQ.0.)GO TO 60                                             00061600
C                                                                       00061700
C     PHYSICAL CONSTANT FOUND, SET KARG = 1                             00061800
C                                                                       00061900
      KARG=1                                                            00062000
      IF(L)999,999,90                                                   00062100
C                                                                       00062200
C     NAME NOT IN PHYSICAL CONSTANT LIST, TRY VARIABLE LIST             00062300
C                                                                       00062400
  60  CALL VARCON(NAM(1))                                               00062500
      IF(ARG.NE.0.)GO TO 80                                             00062600
      CALL ERROR(8)                                                     00062700
  70  KARG=1                                                            00062800
      RETURN                                                            00062900
  80  KARG=3                                                            00063000
   90 I = M                                                             00063100
      IF(NONBLA(I).NE.40)GO TO 999                                      00063200
 100  M=M+1                                                             00063300
C                                                                       00063400
C     CHECK THAT THE NUMBER OF ASTERISKS AT THE END OF THE EXPRESSION   00063500
C     IS THE SAME AS AT THE BEGINNING. L=0 MEANS 1, L=1 MEANS 2         00063600
C                                                                       00063700
      IF(L.NE.0)IF(KARD(M)-40)110,999,110                               00063800
      IF(KARD(M).NE.40.OR.KARD(M+1).EQ.40)GO TO 999                     00063900
      M=M+1                                                             00064000
 110  KARG=KARG+L                                                       00064100
 120  RETURN                                                            00064200
 999  CALL ERROR(7)                                                     00064300
      GO TO 70                                                          00064400
      END                                                               00064500
C   8  44      SUBROUTINE BEGIN                    2 19 68              00064600
      SUBROUTINE BEGIN                                                  00064700
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00064800
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00064900
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00065000
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00065100
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00065200
      DIMENSION ARGS(100)                                               00065300
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00065400
C                                                                       00065500
C     THIS SUBROUTINE CONTAINS THE CODING FOR BEGIN AND SCAN, AN        00065600
C     ARGITRARY GROUPING.                                               00065700
C                                                                       00065800
      IF( L2 .EQ. 1 ) GO TO 50                                          00065900
C                                                                       00066000
C     SCAN  (CARD UP TO AND INCLUDING CARD COLUMN ++ )                  00066100
      IF(NARGS .EQ. 1 .AND. KIND(1) .EQ. 0 .AND. IARGS(1) .GE. 6 .AND.  00066200
     1 IARGS(1) .LE. 80 ) GO TO 10                                      00066300
      K = 205                                                           00066400
      GO TO 60                                                          00066500
  10  KRDEND = IARGS( 1 )                                               00066600
      GO TO 20                                                          00066700
C                                                                       00066800
C     BEGIN STORING INSTRUCTIONS AT NUMBER ++                           00066900
C     IF NO NUMBER IS GIVEN, 1 IS ASSUMED.                              00067000
C                                                                       00067100
  50  IF( MODE .EQ. 1 ) GO TO 70                                        00067200
      K = 5                                                             00067300
  60  CALL ERROR( K )                                                   00067400
  20  RETURN                                                            00067500
  70  IF( NARGS - 1 ) 90, 100, 80                                       00067600
  80  K = 10                                                            00067700
      GO TO 60                                                          00067800
  90  NSTMT = 0                                                         00067900
  95  MODE = 3                                                          00068000
      GO TO 20                                                          00068100
 100  IF ( KIND(1) . EQ. 0 ) GO TO 110                                  00068200
      K = 20                                                            00068300
      GO TO 60                                                          00068400
 110  IF( IARGS( 1 ) .GT. 0 .AND. IARGS( 1 ) .LT. 1000 ) GO TO 120      00068500
      K = 7                                                             00068600
      GO TO 60                                                          00068700
 120  NSTMT = 10 * ( IARGS( 1 ) - 1 )                                   00068800
      GO TO 95                                                          00068900
      END                                                               00069000
C   9  80      SUBROUTINE BEJN                     2 19 68              00069100
      SUBROUTINE BEJN                                                   00069200
      COMMON /RJN/R(100),Z,J                                            00069300
      DOUBLE PRECISION X,R,Z,A,B,C,D,E,F,G,P,Q,Y                        00069400
      Y=1.D0                                                            00069500
  102 X=Z                                                               00069600
      DO 101 N=1,100                                                    00069700
  101 R(N)=0.0                                                          00069800
      LA=0                                                              00069900
      IF (X.LE.60.) GO TO 10                                            00070000
      LA=1                                                              00070100
      IF (X.LE.100.) GO TO 9                                            00070200
      WRITE (6,31)                                                      00070300
   31 FORMAT (37H  X GREATER THAN 100. NO VALUES GIVEN)                 00070400
      GO TO 25                                                          00070500
    9 X=X/2.D0                                                          00070600
   10 A=X/2.D0                                                          00070700
      IF (X.GT.15.) GO TO 5                                             00070800
      B=1.D0                                                            00070900
      C=1.D0                                                            00071000
      DO 1 N=1,30                                                       00071100
      J=N                                                               00071200
      B=B*A/C                                                           00071300
      C=C+1.D0                                                          00071400
      IF (B.LT..5D-9) GO TO 2                                           00071500
    1 CONTINUE                                                          00071600
    2 D=B*A/C                                                           00071700
      A=A**2                                                            00071800
      K=X+6.D0                                                          00071900
      E=K                                                               00072000
      F=K+J                                                             00072100
      G=F+1.D0                                                          00072200
      P=1.D0                                                            00072300
      Q=1.D0                                                            00072400
      DO 3 N=1,K                                                        00072500
      P=1.D0-P*A/(E*F)*Y                                                00072600
      Q=1.D0-Q*A/(E*G)*Y                                                00072700
      E=E-1.D0                                                          00072800
      F=F-1.D0                                                          00072900
    3 G=G-1.D0                                                          00073000
      R(J+1)=B*P                                                        00073100
      R(J+2)=D*Q                                                        00073200
   20 DO 4 N=1,J                                                        00073300
      K=J-N+1                                                           00073400
      A=K                                                               00073500
    4 R(K)=2.D0*A*R(K+1)/X-R(K+2)*Y                                     00073600
      IF (LA.EQ.0) GO TO 25                                             00073700
      LA=LA-1                                                           00073800
      A=R(1)*R(100)                                                     00073900
      B=.0D0                                                            00074000
      DO 11 N=1,99                                                      00074100
      K=100-N                                                           00074200
      A=A+R(N+1)*R(K)                                                   00074300
   11 B=B+R(N)*R(K)                                                     00074400
      J=98                                                              00074500
      R(100)=A                                                          00074600
      R(99)=B                                                           00074700
      X=Z                                                               00074800
      GO TO 20                                                          00074900
    5 K=1.5*X                                                           00075000
      B=1.D0                                                            00075100
      C=K                                                               00075200
      DO 6 N=1,K                                                        00075300
      B=A*B/C                                                           00075400
    6 C=C-1.D0                                                          00075500
      P=2.D-9                                                           00075600
      IF (LA.EQ.1) P=5.D-20                                             00075700
      C=K+1                                                             00075800
      DO 7 N=1,30                                                       00075900
      J=K+N                                                             00076000
      B=B*A/C                                                           00076100
      C=C+1.D0                                                          00076200
      IF (B.LT.P) GO TO 2                                               00076300
      IF (J.EQ.98) GO TO 2                                              00076400
    7 CONTINUE                                                          00076500
      GO TO 2                                                           00076600
   25 RETURN                                                            00076700
      ENTRY BEIN                                                        00076800
      Y=-1.D0                                                           00076900
      GO TO 102                                                         00077000
      END                                                               00077100
C  10 265      SUBROUTINE BESSEL                   2 19 68              00077200
      SUBROUTINE BESSEL                                                 00077300
      COMMON /BLOCKD/R(10100),IA(100),KI(100),ART(100),NR,NRO,NC,NARGS, 00077400
     1VWX(8),NER                                                        00077500
      DIMENSION ARGS(100)                                               00077600
      EQUIVALENCE( ARGS(1), R(10001) )                                  00077700
      DOUBLE PRECISION X,Y,A(200),B(200),W(200),Z,E,P,Q,S,T             00077800
      COMMON / RJN / Z,W                                                00077900
      COMMON / BEZON / A,B,K,L                                          00078000
      COMMON / ABEKI / X,Y,P,Q,S,T                                      00078100
      COMMON /BLOCKE/NAME(4),L1,L2,ISR                                  00078200
C                                                                       00078300
C        WJG PATCH   )())()()())()()()()()()()()()()()()()              00078400
C                                                                       00078500
      IF( NARGS .GE. 3 ) IF( KI  ( 1 ) ) 8805, 8808, 8805               00078600
      CALL ERROR( 10 )                                                  00078700
8802  RETURN                                                            00078800
8805  CALL ERROR( 3 )                                                   00078900
      GO TO 8802                                                        00079000
8808  L2 = IA   ( 1 )                                                   00079100
      IF( L2 .LT. 1 .OR. L2 .GT. 37 ) GO TO 8805                        00079200
      DO 8812 I = 2, NARGS                                              00079300
      KI  ( I-1 ) = KI  ( I )                                           00079400
      IA   ( I-1 ) = IA   ( I )                                         00079500
8812  ARGS( I-1 ) = ARGS( I )                                           00079600
      NARGS = NARGS - 1                                                 00079700
C                                                                       00079800
C         END PATCH    )()()()()()()()()())()()()()()                   00079900
C                                                                       00080000
      IF (L2.GT.28) GO TO 20                                            00080100
      N=0                                                               00080200
      L=L2/2                                                            00080300
      L=2*L                                                             00080400
      IF (L.EQ.L2) N=1                                                  00080500
      IF (L2.GT.12) GO TO 10                                            00080600
      IF (NARGS.GT.2) CALL ERROR(10)                                    00080700
      CALL ADRESS(NARGS,J)                                              00080800
      IF (J.LE.0) CALL ERROR(11)                                        00080900
      LT=1                                                              00081000
      IF (KI(1).EQ.1) GO TO 1                                           00081100
      CALL ADRESS (1,JA)                                                00081200
      LT=2                                                              00081300
    1 M=1                                                               00081400
      IF (L2.GT.2) M=5                                                  00081500
      IF (L2.GT.4) M=3                                                  00081600
      IF (L2.GT.6) M=7                                                  00081700
      IF (L2.GT.8) M=3                                                  00081800
      IF (L2.GT.10) M=7                                                 00081900
      L=0                                                               00082000
      IF (L2.GT.4) L=1                                                  00082100
      IF (L2.GT.8) L=2                                                  00082200
      K=0                                                               00082300
      IF (LT.EQ.1) GO TO 6                                              00082400
      DO 4 I=1,NR                                                       00082500
      X=R(JA)                                                           00082600
      JA=JA+1                                                           00082700
      Y=1.D0                                                            00082800
      IF (L.EQ.0) GO TO 3                                               00082900
      IF (L.EQ.2) GO TO 2                                               00083000
      IF (DABS(X).LT.85.) GO TO 3                                       00083100
      K=K+1                                                             00083200
    2 IF (DABS(X).GT.700.) GO TO 3                                      00083300
      Y=DEXP(X)                                                         00083400
      IF (M.EQ.3) Y=1.D0/Y                                              00083500
    3 R(J)=Y*DBEJ(X,N,M)                                                00083600
    4 J=J+1                                                             00083700
    5 IF (K.NE.0) CALL ERROR(105)                                       00083800
      RETURN                                                            00083900
    6 X=ARGS(1)                                                         00084000
      Y=1.D0                                                            00084100
      IF (L.EQ.0) GO TO 8                                               00084200
      IF (L.EQ.2) GO TO 7                                               00084300
      IF (DABS(X).LT.85.) GO TO 8                                       00084400
      K=1                                                               00084500
    7 IF (DABS(X).GT.700.) GO TO 8                                      00084600
      Y=DEXP(X)                                                         00084700
      IF (M.EQ.3) Y=1.D0/Y                                              00084800
    8 X=Y*DBEJ(X,N,M)                                                   00084900
      DO 9 I=1,NR                                                       00085000
      R(J)=X                                                            00085100
    9 J=J+1                                                             00085200
      GO TO 5                                                           00085300
   10 IF (L2.GT.20) GO TO 30                                            00085400
      IF (NARGS.GT.3) CALL ERROR(10)                                    00085500
      M=1                                                               00085600
      IF (L2.GT.14) M=2                                                 00085700
      IF (L2.GT.16) M=1                                                 00085800
      IF (L2.GT.18) M=2                                                 00085900
      L=0                                                               00086000
      IF (L2.GT.16) L=1                                                 00086100
      Y=.785398163397D0                                                 00086200
      LV=0                                                              00086300
      JX=0                                                              00086400
   11 CALL ADRESS(NARGS,J2)                                             00086500
      IF (J2.LE.0) CALL ERROR(11)                                       00086600
      CALL ADRESS (NARGS-1,J1)                                          00086700
      IF (J1.LE.0) CALL ERROR(11)                                       00086800
      LT=0                                                              00086900
      IF (KI(1).EQ.1) GO TO 12                                          00087000
      CALL ADRESS (1,JA)                                                00087100
      LT=1                                                              00087200
   12 K=0                                                               00087300
      KA=0                                                              00087400
      IF (LT+LV.EQ.0) GO TO 19                                          00087500
      IF (LV.EQ.0) GO TO 32                                             00087600
      IF (LT.EQ.0) GO TO 33                                             00087700
  111 DO 17 I=1,NR                                                      00087800
      IF (KA.EQ.0) X=R(JA)                                              00087900
      JA=JA+1                                                           00088000
      E=1.D0                                                            00088100
      IF (JX.NE.0) Y=R(JB)                                              00088200
      JB=JB+1                                                           00088300
      IF(M.EQ.2) CALL CBEK                                              00088400
      IF(M.EQ.1) CALL CBEI                                              00088500
      Z=X*DCOS(Y)                                                       00088600
      IF (L.EQ.1) GO TO 13                                              00088700
      IF (DABS(Z).LT.85.) GO TO 14                                      00088800
      K=K+1                                                             00088900
   13 E=DEXP(Z)                                                         00089000
      IF (M.EQ.1) E=1.D0/E                                              00089100
   14 IF (N.EQ.0) GO TO 15                                              00089200
      R(J1)=E*S                                                         00089300
      R(J2)=E*T                                                         00089400
      GO TO 16                                                          00089500
   15 R(J1)=E*P                                                         00089600
      R(J2)=E*Q                                                         00089700
   16 J1=J1+1                                                           00089800
   17 J2=J2+1                                                           00089900
   18 IF (K.NE.0) CALL ERROR(105)                                       00090000
      RETURN                                                            00090100
   19 IF (JX.EQ.0) GO TO 33                                             00090200
      Y=ARGS(2)                                                         00090300
      X=ARGS(1)                                                         00090400
      KA=1                                                              00090500
      JX=0                                                              00090600
      GO TO 111                                                         00090700
   30 IF (NARGS.GT.4) CALL ERROR(10)                                    00090800
      JX=1                                                              00090900
      LV=0                                                              00091000
      IF (KI(2).EQ.1) GO TO 31                                          00091100
      CALL ADRESS (2,JB)                                                00091200
      IF (JB.LE.0) CALL ERROR(11)                                       00091300
      LV=1                                                              00091400
   31 M=1                                                               00091500
      IF (L2.GT.22) M=2                                                 00091600
      IF (L2.GT.24) M=1                                                 00091700
      IF (L2.GT.26) M=2                                                 00091800
      L=0                                                               00091900
      IF (L2.GT.24) L=1                                                 00092000
      GO TO 11                                                          00092100
   32 IF (JX.EQ.0) GO TO 111                                            00092200
      Y=ARGS(2)                                                         00092300
      JX=0                                                              00092400
      GO TO 111                                                         00092500
   33 KA=1                                                              00092600
      X=ARGS(1)                                                         00092700
      GO TO 111                                                         00092800
   20 IF (L2.GT.32) GO TO 27                                            00092900
  200 IF (NARGS.GT.2) CALL ERROR (10)                                   00093000
      CALL ADRESS(NARGS,J)                                              00093100
      IF (J.LE.0) CALL ERROR(11)                                        00093200
      LT=0                                                              00093300
      IF (KI(1).EQ.1) GO TO 21                                          00093400
      CALL ADRESS (1,JA)                                                00093500
      IF (JA.LE.0) CALL ERROR(11)                                       00093600
      LT=1                                                              00093700
   21 IF (LT.EQ.0) X=ARGS(1)                                            00093800
      IF (L2.GT.37) GO TO 25                                            00093900
      IF (L2.EQ.32) GO TO 25                                            00094000
      IF (L2.GT.29) GO TO 23                                            00094100
      DO 22 N=1,NR                                                      00094200
      IF (LT.EQ.1) X=R(JA)                                              00094300
      JA=JA+1                                                           00094400
      R(J)=BINTJ0(X)                                                    00094500
   22 J=J+1                                                             00094600
      RETURN                                                            00094700
   23 K=1                                                               00094800
      IF (L2.EQ.31) K=2                                                 00094900
      DO 24 N=1,NR                                                      00095000
      IF (LT.EQ.1) X=R(JA)                                              00095100
      JA=JA+1                                                           00095200
      R(J)=COMELL(X,K)                                                  00095300
   24 J=J+1                                                             00095400
      RETURN                                                            00095500
   25 IF (LT.EQ.1) CALL ERROR (20)                                      00095600
      Z=X                                                               00095700
      IF (L2.EQ.32) CALL BEJN                                           00095800
      IF (L2.EQ.38) CALL BEIN                                           00095900
      IF (L2.EQ.39) GO TO 49                                            00096000
      K=NR                                                              00096100
      IF (K.GT.100) K=100                                               00096200
      DO 26 N=1,K                                                       00096300
      R(J)=W(N)                                                         00096400
   26 J=J+1                                                             00096500
      RETURN                                                            00096600
   27 IF (L2.GT.34) GO TO 42                                            00096700
      L=NR                                                              00096800
      IF (NR.GT.200) L=200                                              00096900
      IF (NARGS.GT.2) CALL ERROR(10)                                    00097000
      CALL ADRESS (NARGS,J)                                             00097100
      IF (J.LE.0) CALL ERROR(11)                                        00097200
      IF (KI(1).EQ.1) CALL ERROR(20)                                    00097300
      CALL ADRESS (1,JA)                                                00097400
      IF (JA.LE.0) CALL ERROR(11)                                       00097500
      IF (L2.EQ.33) GO TO 29                                            00097600
      CALL BEZONE                                                       00097700
      GO TO 28                                                          00097800
   29 CALL BEZERO                                                       00097900
   28 DO 40 N=1,L                                                       00098000
      R(JA)=A(N)                                                        00098100
      R(J)=B(N)                                                         00098200
      JA=JA+1                                                           00098300
   40 J=J+1                                                             00098400
      RETURN                                                            00098500
   42 IF (L2.GT.36) GO TO 46                                            00098600
      IF (NARGS.GT.2) CALL ERROR(10)                                    00098700
      CALL ADRESS(NARGS,J)                                              00098800
      IF (J.LE.0) CALL ERROR(11)                                        00098900
      LT=0                                                              00099000
      IF (KI(1).EQ.1) GO TO 43                                          00099100
      CALL ADRESS (1,JA)                                                00099200
      IF (JA.LE.0) CALL ERROR(11)                                       00099300
      LT=1                                                              00099400
   43 IF (LT.EQ.0) X=ARGS(1)                                            00099500
      K=0                                                               00099600
      IF (L2.EQ.36) K=1                                                 00099700
      DO 45 N=1,NR                                                      00099800
      IF (LT.NE.0) X=R(JA)                                              00099900
      JA=JA+1                                                           00100000
      CALL STRUVE (X,Y,Z)                                               00100100
      IF (K.EQ.0) GO TO 44                                              00100200
      R(J)=Z                                                            00100300
      GO TO 45                                                          00100400
   44 R(J)=Y                                                            00100500
   45 J=J+1                                                             00100600
      RETURN                                                            00100700
   46 IF (L2.GT.37) GO TO 200                                           00100800
      IF (NARGS.GT.3) CALL ERROR(10)                                    00100900
      CALL ADRESS (NARGS,J)                                             00101000
      IF (J.LE.0) CALL ERROR(11)                                        00101100
      IF (KI(1).EQ.1) CALL ERROR (20)                                   00101200
      CALL ADRESS (1,JA)                                                00101300
      IF (JA.LE.0) CALL ERROR(11)                                       00101400
      JB=IA(2)                                                          00101500
      K=IA(2)                                                           00101600
      DO 47 N=1,NR                                                      00101700
      A(N)=R(JA)                                                        00101800
   47 JA=JA+1                                                           00101900
      CALL FOURIA                                                       00102000
      DO 48 N=1,JB                                                      00102100
      R(J)=B(N)                                                         00102200
   48 J=J+1                                                             00102300
      RETURN                                                            00102400
   49 A(1)=DBEJ(X,0,7)                                                  00102500
      A(2)=DBEJ(X,1,7)                                                  00102600
      R(J)=A(1)                                                         00102700
      R(J+1)=A(2)                                                       00102800
      J=J+2                                                             00102900
      DO 50 I=3,NR                                                      00103000
      Z=I-2                                                             00103100
      A(I)=A(I-2)+2.*Z*A(I-1)/X                                         00103200
      R(J)=A(I)                                                         00103300
   50 J=J+1                                                             00103400
      RETURN                                                            00103500
      END                                                               00103600
C  11  59      SUBROUTINE BEZERO                   2 19 68              00103700
      SUBROUTINE BEZERO                                                 00103800
      DOUBLE PRECISION A(200),B(200),X,Y,AA,AB,AC                       00103900
      COMMON /BEZON/A,B,M,L                                             00104000
      KB=1                                                              00104100
      N=M                                                               00104200
   25 J=4*N-1                                                           00104300
      IF (J.GT.44) GO TO 20                                             00104400
      GO TO (1,2,3,4,5,6,7,8,9,10,11),N                                 00104500
    1 X=2.404825577D0                                                   00104600
      Y=.5191474973D0                                                   00104700
      GO TO 30                                                          00104800
    2 X=5.5200781103D0                                                  00104900
      Y=-.3402648065D0                                                  00105000
      GO TO 30                                                          00105100
    3 X=8.6537279129D0                                                  00105200
      Y=.2714522999D0                                                   00105300
      GO TO 30                                                          00105400
    4 X=11.7915344391D0                                                 00105500
      Y=-.2324598314D0                                                  00105600
      GO TO 30                                                          00105700
    5 X=14.9309177086D0                                                 00105800
      Y=.2065464331D0                                                   00105900
      GO TO 30                                                          00106000
    6 X=18.0710639679D0                                                 00106100
      Y=-.187728803D0                                                   00106200
      GO TO 30                                                          00106300
    7 X=21.2116366299D0                                                 00106400
      Y=.1732658942D0                                                   00106500
      GO TO 30                                                          00106600
    8 X=24.3524715308D0                                                 00106700
      Y=-.1617015507D0                                                  00106800
      GO TO 30                                                          00106900
    9 X=27.493479132D0                                                  00107000
      Y=.1521812138D0                                                   00107100
      GO TO 30                                                          00107200
   10 X=30.6346064684D0                                                 00107300
      Y=-.1441659777D0                                                  00107400
      GO TO 30                                                          00107500
   11 X=33.7758202136D0                                                 00107600
      Y=.1372969434D0                                                   00107700
      GO TO 30                                                          00107800
   20 X=J                                                               00107900
      X=X*3.1415926536D0                                                00108000
      AA=1.D0/X**2                                                      00108100
      AB=1.D0+2.D0*AA*(1.D0-AA*(31.D0-AA*(3779.D0-AA*6277237.D0/7)/5.D0)00108200
     1/3.D0)                                                            00108300
      J=N/2                                                             00108400
      J=2*J                                                             00108500
      AC=1.D0                                                           00108600
      IF (J.EQ.N) AC=-1.D0                                              00108700
      Y=AC*1.595769122D0*(1.-AA**2*56.D0/3.D0)/DSQRT(X)                 00108800
      X=X*AB/4.D0                                                       00108900
   30 A(KB)=X                                                           00109000
      B(KB)=Y                                                           00109100
      N=N+1                                                             00109200
      KB=KB+1                                                           00109300
      IF (KB.LE.L) GO TO 25                                             00109400
      RETURN                                                            00109500
      END                                                               00109600
C  12  58      SUBROUTINE BEZONE                   2 19 68              00109700
      SUBROUTINE BEZONE                                                 00109800
      DOUBLE PRECISION A(200),B(200),R,S,T,X,Y                          00109900
      COMMON /BEZON/A,B,M,L                                             00110000
      KB=1                                                              00110100
      N=M                                                               00110200
   25 J=4*N+1                                                           00110300
      IF (J.GT.46) GO TO 20                                             00110400
      GO TO (1,2,3,4,5,6,7,8,9,10,11),N                                 00110500
    1 X=3.8317059702D0                                                  00110600
      Y=-.4027593957D0                                                  00110700
      GO TO 30                                                          00110800
    2 X=7.0155866698D0                                                  00110900
      Y= .3001157525D0                                                  00111000
      GO TO 30                                                          00111100
    3 X=10.1734681351D0                                                 00111200
      Y= -.2497048771D0                                                 00111300
      GO TO 30                                                          00111400
    4 X=13.3236919363D0                                                 00111500
      Y=  .2183594072D0                                                 00111600
      GO TO 30                                                          00111700
    5 X=16.4706300509D0                                                 00111800
      Y= -.1964653715D0                                                 00111900
      GO TO 30                                                          00112000
    6 X=19.6158585105D0                                                 00112100
      Y=  .180063375D0                                                  00112200
      GO TO 30                                                          00112300
    7 X=22.7600843806D0                                                 00112400
      Y= -.1671846005D0                                                 00112500
      GO TO 30                                                          00112600
    8 X=25.9036720876D0                                                 00112700
      Y=  .1567249863D0                                                 00112800
      GO TO 30                                                          00112900
    9 X=29.0468285349D0                                                 00113000
      Y= -.1480111100D0                                                 00113100
      GO TO 30                                                          00113200
   10 X=32.1896799110D0                                                 00113300
      Y=  .1406057982D0                                                 00113400
      GO TO 30                                                          00113500
   11 X=35.3323075501D0                                                 00113600
      Y= -.1342112403D0                                                 00113700
      GO TO 30                                                          00113800
   20 X=J                                                               00113900
      X=X*3.1415926536D0                                                00114000
      R=1.D0/X**2                                                       00114100
      S=1.D0-6.D0*R*(1.D0-R*(1.D0-R*(157.2D0-130080.6D0*R/7.D0)))       00114200
      J=N/2                                                             00114300
      J=2*J                                                             00114400
      T=1.D0                                                            00114500
      IF (J.NE.N) T=-1.D0                                               00114600
      Y=T*1.595769122D0*(1.D0+R**2*24.D0*(1.D0-81.6*R))/DSQRT(X)        00114700
      X=S*X/4.D0                                                        00114800
   30 A(KB)=X                                                           00114900
      B(KB)=Y                                                           00115000
      N=N+1                                                             00115100
      KB=KB+1                                                           00115200
      IF (KB.LE.L) GO TO 25                                             00115300
      RETURN                                                            00115400
      END                                                               00115500
C  13  21      FUNCTION BINTJ0(X)                  2 19 68              00115600
      DOUBLE PRECISION FUNCTION BINTJ0(X)                               00115700
      COMMON /RJN/A(100),Z                                              00115800
      DOUBLE PRECISION A,Z,X,B,C                                        00115900
      Z=DABS(X)                                                         00116000
      IF (Z.GT.100.) GO TO 20                                           00116100
      CALL BEJN                                                         00116200
      IF (Z.GT.60.) GO TO 2                                             00116300
      B=.0D0                                                            00116400
      DO 1 N=2,100,2                                                    00116500
    1 B=B+A(N)                                                          00116600
      B=2.D0*B                                                          00116700
      GO TO 3                                                           00116800
   20 A(1)=DBEJ(Z,0,7)                                                  00116900
      A(2)=DBEJ(Z,1,7)                                                  00117000
    2 C=1.D0/Z**2                                                       00117100
      B=1.D0+A(2)*(1.D0-C*(1.D0-C*(9.D0-C*(225.D0-C*11025.D0))))        00117200
      C=1.D0-C*(3.D0-C*(45.D0-C*(1575.D0-99225.D0*C)))                  00117300
      B=B-A(1)*C/Z                                                      00117400
    3 BINTJ0=B                                                          00117500
      RETURN                                                            00117600
      END                                                               00117700
C  14  45      BLOCK DATA                          2 19 68              00117800
      BLOCK DATA                                                        00117900
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00118000
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00118100
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00118200
      COMMON / ABCDEF / L( 48 )                                         00118300
      COMMON / PCONST / P( 40 ), N( 40 )                                00118400
C                                                                       00118500
C     THIS BLOCK DEFINES CONSTANTS TO BE USED THROUGHOUT OMNITAB        00118600
C     WHOSE VALUE (ACCURACY) WILL HAVE TO BE CHANGED IN GOING FROM      00118700
C     ONE SIZE FLOATING POINT NUMBER TO ANOTHER                         00118800
C                                                                       00118900
      DATA PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG/3.14159265359,          00119000
     12.71828182846,1.57079632679,57.295779513,1.74532925199E-2,158.,   00119100
     2.8E6,549755813887./                                               00119200
      DATA NMCARD / 15*1H ,1HY,1HO,1HU,1H ,1HD,1HU,2*1HM,1HY,1H,,1H ,   00119300
     1 1HY,1HO,1HU,1H ,1HL,1HE,1HF,1HT,1H ,1HO,2*1HF,1H ,1HT,1HH,1HE,   00119400
     2 1H ,1H-,1HO,1HM,1HN,1HI,1HT,1HA,1HB,1H-,1H ,1HC,1HA,1HR,1HD,     00119500
     3 15*1H  /                                                         00119600
      DATA  L(1),L(2),L(3),L(4),L(5),L(6),L(7),L(8),L(9),L(10)/         00119700
     1 1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/                         00119800
      DATA  L(11),L(12),L(13),L(14),L(15),L(16),L(17),L(18),L(19),L(20)/00119900
     1 1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI,1HJ/                         00120000
      DATA  L(21),L(22),L(23),L(24),L(25),L(26),L(27),L(28),L(29),L(30)/00120100
     1 1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR,1HS,1HT/                         00120200
      DATA  L(31),L(32),L(33),L(34),L(35),L(36),L(37),L(38),L(39),L(40)/00120300
     1 1HU,1HV,1HW,1HX,1HY,1HZ,1H/,1H.,1H-,1H+/                         00120400
      DATA  L(41),L(42),L(43),L(44),L(45),L(46),L(47),L(48)/            00120500
     1 1H*,1H(,1H),1H,,1H ,1H=,1H",1H$/                                 00120600
      DATA P(1),P(2),P(3),P(4),P(5),P(6),P(7),P(8),P(9),P(10)/          00120700
     12*3.14159265359,2*2.71828182846,2.997925E8,2.997925E10,           00120800
     2 1.60210E-19,1.60210E-20,2*6.02252E23/                            00120900
      DATA P(11),P(12),P(13),P(14),P(15),P(16),P(17),P(18),P(19),P(20)/ 00121000
     1 9.1091E-31,9.1091E-28,1.67252E-27,1.67252E-24,9.64870E4,9648.70, 00121100
     2 66.256E-34,6.6256E-27,2*7.29720E-3/                              00121200
      DATA P(21),P(22),P(23),P(24),P(25),P(26),P(27),P(28),P(29),P(30)/ 00121300
     1 1.758796E11,17587960.,10973731.,109737.31,2.67519E8,26751.9,     00121400
     2 9.2732E-24,9.2732E-21,8.3143,8.3143E7/                           00121500
      DATA P(31),P(32),P(33),P(34),P(35),P(36),P(37),P(38),P(39),P(40)/ 00121600
     1 1.38054E-23,1.38054E-16,3.7405E-16,3.7405E-5,1.43879E-2,1.43879, 00121700
     2 5.6697E-8,5.6697E-5,6.670E-11,6.670E-8/                          00121800
C                                                                       00121900
      DATA N(1),N(2),N(3),N(4),N(5),N(6),N(7),N(8),N(9),N(10)/          00122000
     1 11907,3645,2187,12393,10206,9612,9909,4374,5832,1069/            00122100
      DATA N(11),N(12),N(13),N(14),N(15),N(16),N(17),N(18),N(19),N(20)/ 00122200
     1 12749,13379,5143,10046,13122,8019,2606,2750,14101,5103/          00122300
      END                                                               00122400
C  15  79      SUBROUTINE CBEI                     2 19 68              00122500
      SUBROUTINE CBEI                                                   00122600
C         COMPUTES I0(Z) AND I1(Z) FOR COMPLEX ARGUMENT  R*E(IS)=Z      00122700
      COMMON /ABEKI/R,S,A,B,C,D                                         00122800
      DOUBLE PRECISION A,B,C,D,E,F,G,H,P,Q,R,S,T,X,Y,Z,V,U,W,AA         00122900
      E=DCOS(S)                                                         00123000
      F=DSIN(S)                                                         00123100
      IF (R.GT.15.5) GO TO 3                                            00123200
      P=1.D0-2.D0*F**2                                                  00123300
      AA=P                                                              00123400
      Q=2.D0*E*F                                                        00123500
      W=Q                                                               00123600
      A=1.D0                                                            00123700
      B=0.D0                                                            00123800
      C=1.D0                                                            00123900
      U=0.D0                                                            00124000
      G=1.D0                                                            00124100
      T=2.D0                                                            00124200
      X=(R/2.D0)**2                                                     00124300
      V=X                                                               00124400
      Y=X                                                               00124500
      DO 1 N=1,60                                                       00124600
      Z=1.D0/G**2                                                       00124700
      H=1.D0/(G*T)                                                      00124800
      A=A+X*Z*P                                                         00124900
      B=B+X*Z*Q                                                         00125000
      C=C+V*H*P                                                         00125100
      U=U+V*H*Q                                                         00125200
      X=X*Y*Z                                                           00125300
      IF (X.LT..5D-10) GO TO 2                                          00125400
      V=V*Y*H                                                           00125500
      Z=P                                                               00125600
      P=Z*AA-Q*W                                                        00125700
      Q=Q*AA+Z*W                                                        00125800
      G=G+1.D0                                                          00125900
    1 T=T+1.D0                                                          00126000
    2 D=R*(C*F+U*E)/2.D0                                                00126100
      C=R*(C*E-U*F)/2.D0                                                00126200
      GO TO 6                                                           00126300
    3 Z=DEXP(R*E)/DSQRT(6.283185307D0*R)                                00126400
      X=S/2.D0-R*F                                                      00126500
      Y=Z*DCOS(X)                                                       00126600
      Z=Z*DSIN(X)                                                       00126700
      W=-1.D0                                                           00126800
      G=1.D0                                                            00126900
      H=3.D0                                                            00127000
      P=E                                                               00127100
      Q=F                                                               00127200
      T=1.D0                                                            00127300
      U=0.D0                                                            00127400
      V=1.D0                                                            00127500
      X=0.D0                                                            00127600
      A=1.D0                                                            00127700
      B=1.D0/(8.D0*R)                                                   00127800
      C=B                                                               00127900
      D=B                                                               00128000
      DO 4 N=1,20                                                       00128100
      AA=B*G**2/A                                                       00128200
      T=T+AA*P                                                          00128300
      U=U+AA*Q                                                          00128400
      AA=C*W*H/A                                                        00128500
      V=V+AA*P                                                          00128600
      X=X+AA*Q                                                          00128700
      B=B*D*G**2/A                                                      00128800
      IF (B.LT..5D-10) GO TO 5                                          00128900
      C=C*D*W*H/A                                                       00129000
      W=W+2.D0                                                          00129100
      G=G+2.D0                                                          00129200
      H=H+2.D0                                                          00129300
      A=A+1.D0                                                          00129400
      AA=P*E-Q*F                                                        00129500
      Q=F*P+E*Q                                                         00129600
    4 P=AA                                                              00129700
    5 A=Y*T-Z*U                                                         00129800
      B=-(Y*U+T*Z)                                                      00129900
      C=Y*V-Z*X                                                         00130000
      D=-(Y*X+Z*V)                                                      00130100
    6 RETURN                                                            00130200
      END                                                               00130300
C  16 102      SUBROUTINE CBEK                     2 19 68              00130400
      SUBROUTINE CBEK                                                   00130500
C       COMPUTES K0(Z) AND K1(Z) FOR COMPLEX ARGUMENT Z=R*E(IS)         00130600
      COMMON /ABEKI/R,S,A,B,C,D                                         00130700
      DOUBLE PRECISION A,B,C,D,E,F,G,H,P,Q,R,S,T,U,V,W,X,Y,Z,           00130800
     1AA(40),AB(40),AC,AD,AE                                            00130900
      E=DCOS(S)                                                         00131000
      F=DSIN(S)                                                         00131100
      IF (R.GT.8.) GO TO 5                                              00131200
      P=1.D0-2.D0*F**2                                                  00131300
      Q=2.D0*E*F                                                        00131400
      W=P                                                               00131500
      Z=Q                                                               00131600
      X=(R/2.D0)**2                                                     00131700
      Y=X                                                               00131800
      V=X                                                               00131900
      G=E*(4.D0*E**2-3.D0)                                              00132000
      H=F*(3.D0-4.D0*F**2)                                              00132100
      T=DLOG(R/2.D0)+.5772156649D0                                      00132200
      A=-T                                                              00132300
      B=-S                                                              00132400
      C=E*(T-0.5D0)-S*F                                                 00132500
      U=F*(T-0.5D0)+S*E                                                 00132600
      AC=1.D0                                                           00132700
      AD=2.D0                                                           00132800
      L=L-10                                                            00132900
      IF (L.GT.1) GO TO 2                                               00133000
      AA(1)=1.D0                                                        00133100
      AB(1)=1.25D0                                                      00133200
      DO 1 N=2,40                                                       00133300
      AE=N                                                              00133400
      AA(N)=AA(N-1)+1.D0/AE                                             00133500
    1 AB(N)=AA(N)+1.D0/(2.D0*(AE+1.D0))                                 00133600
    2 L=20                                                              00133700
      DO 3 N=1,40                                                       00133800
      AE=T-AA(N)                                                        00133900
      D=P*AE-S*Q                                                        00134000
      AE=Q*AE+S*P                                                       00134100
      A=A-D*X/AC**2                                                     00134200
      B=B-AE*X/AC**2                                                    00134300
      AE=T-AB(N)                                                        00134400
      D=G*AE-H*S                                                        00134500
      AE=H*AE+G*S                                                       00134600
      C=C+D*Y/(AC*AD)                                                   00134700
      U=U+AE*Y/(AC*AD)                                                  00134800
      X=X*V/AC**2                                                       00134900
      IF (X.LT..5D-10) GO TO 4                                          00135000
      Y=Y*V/(AC*AD)                                                     00135100
      AC=AC+1.D0                                                        00135200
      AD=AD+1.D0                                                        00135300
      AE=P                                                              00135400
      P=AE*W-Q*Z                                                        00135500
      Q=Q*W+AE*Z                                                        00135600
      AE=G                                                              00135700
      G=AE*W-H*Z                                                        00135800
    3 H=H*W+AE*Z                                                        00135900
    4 C=E/R+R*C/2.D0                                                    00136000
      D=-F/R+R*U/2.D0                                                   00136100
      GO TO 8                                                           00136200
    5 U=DEXP(-R*E)*DSQRT(1.5707963268D0/R)                              00136300
      V=R*F+S/2.D0                                                      00136400
      Y=U*DCOS(V)                                                       00136500
      Z=U*DSIN(V)                                                       00136600
      W=-1.D0                                                           00136700
      G=1.D0                                                            00136800
      H=3.D0                                                            00136900
      P=E                                                               00137000
      Q=F                                                               00137100
      T=1.D0                                                            00137200
      U=0.D0                                                            00137300
      V=1.D0                                                            00137400
      X=0.D0                                                            00137500
      A=1.D0                                                            00137600
      B=1.D0/(8.D0*R)                                                   00137700
      C=B                                                               00137800
      D=B                                                               00137900
      AC=-1.D0                                                          00138000
      DO 6 N=1,12                                                       00138100
      AD=AC*B*G**2/A                                                    00138200
      AE=AC*C*W*H/A                                                     00138300
      T=T+AD*P                                                          00138400
      U=U-AD*Q                                                          00138500
      V=V+AE*P                                                          00138600
      X=X-AE*Q                                                          00138700
      AD=B                                                              00138800
      B=B*D*G**2/A                                                      00138900
      IF (B.GT.AD) GO TO 7                                              00139000
      IF (B.LT..5D-10) GO TO 7                                          00139100
      C=C*D*W*H/A                                                       00139200
      W=W+2.D0                                                          00139300
      H=H+2.D0                                                          00139400
      G=G+2.D0                                                          00139500
      A=A+1.D0                                                          00139600
      AC=-1.D0*AC                                                       00139700
      AD=P                                                              00139800
      P=AD*E-Q*F                                                        00139900
    6 Q=Q*E+AD*F                                                        00140000
    7 A=Y*T+U*Z                                                         00140100
      B=Y*U-T*Z                                                         00140200
      C=Y*V+X*Z                                                         00140300
      D=Y*X-V*Z                                                         00140400
    8 RETURN                                                            00140500
      END                                                               00140600
C  17  24      SUBROUTINE CHANGE                   2 19 68              00140700
      SUBROUTINE CHANGE                                                 00140800
C                                                                       00140900
C CHANGE SIGNS OF COLS ++, ++, ++, ETC.                                 00141000
C                                                                       00141100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00141200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00141300
      DIMENSION ARGS(100)                                               00141400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00141500
      IF (NARGS) 910,910,5                                              00141600
    5 DO 20 I=1,NARGS                                                   00141700
      CALL ADRESS( I, J )                                               00141800
      IF(J) 903,911,10                                                  00141900
   10 IF (NERROR .NE. 0) RETURN                                         00142000
      DO 20 N=1,NRMAX                                                   00142100
      JJ=J+N-1                                                          00142200
   20 RC(JJ)=-RC(JJ)                                                    00142300
      GO TO 999                                                         00142400
  903 CALL ERROR (3)                                                    00142500
      GO TO 999                                                         00142600
  910 CALL ERROR (10)                                                   00142700
      GO TO 999                                                         00142800
  911 CALL ERROR (11)                                                   00142900
  999 RETURN                                                            00143000
      END                                                               00143100
C  18  19      SUBROUTINE CHKCOL( J )              2 19 68              00143200
      SUBROUTINE CHKCOL( J )                                            00143300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00143400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00143500
      DIMENSION ARGS(100)                                               00143600
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00143700
C                                                                       00143800
C         THIS ROUTINE CHECKS THAT ALL "NARGS" ARGUMENTS ARE LEGAL      00143900
C         COLUMN NUMBERS AND CONVERTS THEM IN IARGS TO THEIR BEGINNING  00144000
C         ADDRESSES.                                                    00144100
      IF( NARGS .GT. 0 ) GO TO 20                                       00144200
  10  J = 1                                                             00144300
      GO TO 40                                                          00144400
  20  DO 30 I = 1, NARGS                                                00144500
      CALL ADRESS( I, IARGS( I ) )                                      00144600
      IF( IARGS( I ) .LE. 0 ) GO TO 10                                  00144700
  30  CONTINUE                                                          00144800
      J = 0                                                             00144900
  40  RETURN                                                            00145000
      END                                                               00145100
C  19  26      SUBROUTINE CKIND(J)                 2 19 68              00145200
      SUBROUTINE CKIND(J)                                               00145300
C**** CKIND                                                             00145400
C**** S PEAVY    5/22/67                                                00145500
C**** THE FIRST J VALUES OF KIND ARE CHECKED                            00145600
C**** IF ALL ARE =0 THEN J=0                                            00145700
C**** IF ALL ARE =1 THEN J=1                                            00145800
C**** IF SOME ARE 0 AND SOME 1   J=2                                    00145900
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00146000
     ONROW,                                                             00146100
     1NCOL,NARGS,VWXYZ(8),NERROR                                        00146200
      DIMENSION ARGS(100)                                               00146300
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00146400
      JA=J                                                              00146500
      J=0                                                               00146600
      DO  10 I=1,JA                                                     00146700
      IF(KIND(I).NE.0) GO TO 15                                         00146800
   10 CONTINUE                                                          00146900
      RETURN                                                            00147000
   15 J=1                                                               00147100
      DO  20 I=1,JA                                                     00147200
      IF(KIND(I).NE.1) GO TO 30                                         00147300
   20 CONTINUE                                                          00147400
      RETURN                                                            00147500
   30 J=2                                                               00147600
      RETURN                                                            00147700
      END                                                               00147800
C  20  46      FUNCTION COMELL (Z,I)               2 19 68              00147900
      DOUBLE PRECISION FUNCTION COMELL (Z,I)                            00148000
C       COMPLETE ELLIPTIC INTEGRALS - FIRST AND SECOND KIND             00148100
      DOUBLE PRECISION Z,X,A,B,C,D,E,P,Q                                00148200
      X=Z                                                               00148300
      IF (DABS(Z).LT.1.D0) GO TO 1                                      00148400
      WRITE (6,2)                                                       00148500
    2 FORMAT (50H MODULUS GREATER OR EQUAL TO 1.+VALUE SET TO .9999)    00148600
      X=.9999                                                           00148700
    1 A=X**2                                                            00148800
      B=DSQRT(1.D0-A)                                                   00148900
      IF (X.GT..996) GO TO 6                                            00149000
      B=(1.D0-B)/(1.D0+B)                                               00149100
      A=B**2                                                            00149200
      B=1.D0+B                                                          00149300
      C=1.D0                                                            00149400
      D=C                                                               00149500
      E=2.D0                                                            00149600
      IF (I.EQ.1) GO TO 3                                               00149700
      B=1.D0/B                                                          00149800
      D=-1.D0                                                           00149900
    3 P=A                                                               00150000
      DO 4 N=1,90                                                       00150100
      C=C+P*(D/E)**2                                                    00150200
      P=P*A*(D/E)**2                                                    00150300
      IF (P.LT..1D-9) GO TO 5                                           00150400
      D=D+2.D0                                                          00150500
    4 E=E+2.D0                                                          00150600
    5 A=B*C*1.570796326D0                                               00150700
      GO TO 8                                                           00150800
    6 A=DLOG(4.D0/B)                                                    00150900
      Q=B**2                                                            00151000
      IF (I.GT.1) GO TO 7                                               00151100
      B=.25D0*(A-1.D0)                                                  00151200
      C=.140625D0*(A-1.666666666D0)                                     00151300
      D=9.765625D-2*(A-1.233333333D0)                                   00151400
      E=1255.D0*(A-1.27904761904D0)/16384.D0                            00151500
      A=A+Q*(B+Q*(C+Q*(D+Q*E)))                                         00151600
      GO TO 8                                                           00151700
    7 B=.5D0*(A-.5D0)                                                   00151800
      C=.1875D0*(A-1.083333333D0)                                       00151900
      D=.1171875D0*(A-1.2D0)                                            00152000
      E=175.D0*(A-1.251190476D0)/2048.D0                                00152100
      A=1.+Q*(B+Q*(C+Q*(D+Q*E)))                                        00152200
    8 COMELL=A                                                          00152300
      RETURN                                                            00152400
      END                                                               00152500
C  21 113      FUNCTION DBEJ(X,N,M)                2 19 68              00152600
      DOUBLE PRECISION FUNCTION DBEJ(X,N,M)                             00152700
      DOUBLE PRECISION E,H,X,A,B,C,D,Y,S(120),T(1200)                   00152800
    1 IF (DABS(X).GT.16.5) GO TO 10                                     00152900
      A=(X/2.D0)**2                                                     00153000
      J=X/.4+6.8                                                        00153100
      B=J                                                               00153200
      C=J+N                                                             00153300
      D=-1.D0                                                           00153400
      IF (M.GT.1) D=1.D0                                                00153500
      IF (M.GT.3) GO TO 3                                               00153600
      Y=1.D0                                                            00153700
      DO 2 I=1,J                                                        00153800
      Y=1.D0+Y*A/(B*C)*D                                                00153900
      B=B-1.D0                                                          00154000
    2 C=C-1.D0                                                          00154100
      IF (N.GT.0) Y=X*Y/2.D0                                            00154200
      GO TO 55                                                          00154300
    3 L=L-10                                                            00154400
      IF (L.GT.0) GO TO 5                                               00154500
      E=1.0D0                                                           00154600
      S(1)=.5772156649015D0                                             00154700
      S(61)=S(1)-.5D0                                                   00154800
      DO 4 I=2,60                                                       00154900
      S(I)=S(I-1)-1.D0/E                                                00155000
      S(I+60)=S(I)-1.D0/(2.D0*(E+1.D0))                                 00155100
    4 E=E+1.D0                                                          00155200
    5 L=100                                                             00155300
      E=DLOG(X/2.D0)                                                    00155400
      DO 6 I=1,120                                                      00155500
    6 T(I)=S(I)+E                                                       00155600
      IF (M.LT.6) GO TO 11                                              00155700
      IF (X.GT.8.) GO TO 10                                             00155800
   11 IA=0                                                              00155900
      IF (N.GT.0) IA=60                                                 00156000
      IF (M.GT.5) D=-1.D0                                               00156100
      I=J+IA+1                                                          00156200
      Y=T(I)                                                            00156300
      DO 7IB=1,J                                                        00156400
      I=J-IB+IA+1                                                       00156500
      Y=T(I)-D*A*Y/(B*C)                                                00156600
      B=B-1.D0                                                          00156700
    7 C=C-1.D0                                                          00156800
      IF (N.GT.0) Y=X*Y/2.D0                                            00156900
      IF (M.GT.5) GO TO 8                                               00157000
      Y=Y*.636619772368D0                                               00157100
      IF (N.NE.0) Y=-.636619772368D0/X+Y                                00157200
      GO TO 55                                                          00157300
    8 Y=-Y                                                              00157400
      IF (N.NE.0) Y=1.D0/X-Y                                            00157500
      GO TO 55                                                          00157600
   10 A=8.D0*X                                                          00157700
      H=N                                                               00157800
      H=(2.*H)**2                                                       00157900
      T(1)=(H-1.D0)/A                                                   00158000
      D=T(1)                                                            00158100
      DO 30 I=2,20                                                      00158200
      K=I                                                               00158300
      B=I                                                               00158400
      C=(2*I-1)**2                                                      00158500
      T(I)=(H-C)/(A*B)                                                  00158600
      E=D                                                               00158700
      D=T(I)*D                                                          00158800
      E=DABS(D/E)                                                       00158900
      IF (DABS(D).LT..5D-10) GO TO 32                                   00159000
      IF (E.GT..91) GO TO 32                                            00159100
   30 T(I+2)=0.0D0                                                      00159200
   32 A=-1.D0                                                           00159300
      IF (M.LE.1) GO TO 20                                              00159400
      IF (M.LE.3) GO TO 12                                              00159500
      IF (M.LE.5) GO TO 20                                              00159600
      A=1.D0                                                            00159700
   12 Y=1.D0                                                            00159800
      DO 14 I=1,K                                                       00159900
      J=K-I+1                                                           00160000
   14 Y=1.D0+A*Y*T(J)                                                   00160100
      A=1.D0                                                            00160200
      IF (X.LT.700.D0) A=DEXP(X)                                        00160300
      IF (M.LE.5) GO TO 16                                              00160400
      Y=Y/(A*DSQRT(.636619772368D0*X))                                  00160500
      GO TO 55                                                          00160600
   16 Y=Y*A/DSQRT(6.283185307D0*X)                                      00160700
      GO TO 55                                                          00160800
   20 Y=DSQRT(3.1415926536D0*X)                                         00160900
      J=K/2                                                             00161000
      K=2*J                                                             00161100
      J=J-1                                                             00161200
      A=1.D0                                                            00161300
      H=A                                                               00161400
      DO 21 I=1,J                                                       00161500
      IA=K-2*I+1                                                        00161600
      A=1.D0-A*T(IA)*T(IA+1)                                            00161700
   21 H=1.D0-H*T(IA)*T(IA-1)                                            00161800
      A=(1.D0-T(1)*T(2)*A)/Y                                            00161900
      H=T(1)*H/Y                                                        00162000
      B=DSIN(X)                                                         00162100
      C=DCOS(X)                                                         00162200
      D=A-H                                                             00162300
      E=A+H                                                             00162400
      IF (M.GT.2) GO TO 24                                              00162500
      IF (N.EQ.0) GO TO 22                                              00162600
      Y=E*B-D*C                                                         00162700
      GO TO 55                                                          00162800
   22 Y=D*B+E*C                                                         00162900
      GO TO 55                                                          00163000
   24 IF (N.EQ.0) GO TO 26                                              00163100
      Y=-D*B-E*C                                                        00163200
      GO TO 55                                                          00163300
   26 Y=E*B-D*C                                                         00163400
      GO TO 55                                                          00163500
   55 DBEJ=Y                                                            00163600
   60 RETURN                                                            00163700
      END                                                               00163800
C  22  10      FUNCTION DBEY( X , N )              2 19 68              00163900
      DOUBLE PRECISION FUNCTION DBEY (X,N)                              00164000
      DOUBLE PRECISION X                                                00164100
C                                                                       00164200
C      THIS IS A DUMMY FUNCTION. THIS FUNCTION WAS LEFT OUT OF          00164300
C   THE LATEST LISTING OF THE OMNITAB PROGRAM FROM THE BUREAU OF        00164400
C   STANDARDS. SO THIS ROUTINE WAS SUBSTITUTED UNTIL THE CORRECT ROUTINE00164500
C   CAN BE OBTAINED.                            R. L. CHAMBERLAIN 2/12/600164600
C                                                                       00164700
      RETURN                                                            00164800
      END                                                               00164900
C  23  47      SUBROUTINE DEFINE                   2 19 68              00165000
      SUBROUTINE DEFINE                                                 00165100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00165200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00165300
      DIMENSION ARGS(100)                                               00165400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00165500
C                                                                       00165600
C     DEFINE $$ INTO ROW ++, COL ++.                                    00165700
C     DEFINE ROW ++, COL ++ INTO ROW ++, COL ++.                        00165800
C     DEFINE ROW ++, COL ++ INTO COL ++.                                00165900
C                                                                       00166000
      IF( NARGS .NE. 3 ) IF( NARGS - 4 ) 200, 40, 200                   00166100
      IF( KIND( 1 ) .EQ. 0 ) GO TO 40                                   00166200
  10  I = NARGS                                                         00166300
      GO TO 100                                                         00166400
  20  IF( NERROR .EQ. 0 ) RC( L ) = ARGS( 1 )                           00166500
  30  RETURN                                                            00166600
  40  I = 2                                                             00166700
      GO TO 100                                                         00166800
  50  ARGS( 1 ) = RC( L )                                               00166900
      IF( NARGS .EQ. 4 ) GO TO 10                                       00167000
      CALL ADRESS( 3, I )                                               00167100
      IF ( I ) 210, 220, 60                                             00167200
  60  IF( NERROR .NE. 0 ) GO TO 30                                      00167300
      IF( NRMAX .EQ. 0 ) GO TO 70                                       00167400
      CALL VECTOR( ARGS( 1 ), I )                                       00167500
      GO TO 30                                                          00167600
  70  I = 9                                                             00167700
  80  CALL ERROR( I )                                                   00167800
      GO TO 30                                                          00167900
C                                                                       00168000
C     CHECK AND CALCULATE WORKSHEET ENTRY LOCATION INTO L               00168100
C                                                                       00168200
 100  CALL ADRESS (I, L )                                               00168300
      IF ( L ) 210, 220, 110                                            00168400
 110  IF( KIND(I-1) .EQ. 0 .AND. IARGS(I-1) .GT. 0 .AND. IARGS(I-1) .LE.00168500
     1 NROW ) GO TO 120                                                 00168600
      I = 16                                                            00168700
      GO TO 80                                                          00168800
 120  L = L + IARGS(I-1) - 1                                            00168900
      IF( I - 2 ) 50, 50, 20                                            00169000
 200  I = 10                                                            00169100
      GO TO 80                                                          00169200
 210  I = 20                                                            00169300
      GO TO 80                                                          00169400
 220  I = 11                                                            00169500
      GO TO 80                                                          00169600
      END                                                               00169700
C  24  21      SUBROUTINE DIMENS                   2 19 68              00169800
      SUBROUTINE DIMENS                                                 00169900
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00170000
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00170100
      DIMENSION ARGS(100)                                               00170200
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00170300
      NRC = 10000                                                       00170400
      IF( NARGS .EQ. 2 ) IF( KIND( 1 ) + KIND( 2 ) ) 30, 40, 30         00170500
      K = 10                                                            00170600
  10  CALL ERROR( K )                                                   00170700
  20  RETURN                                                            00170800
  30  K = 20                                                            00170900
      GO TO 10                                                          00171000
  40  IF( IARGS( 1 ) .GT. 0 .AND. IARGS( 2 ) .GT. 0 .AND. IARGS( 1 ) *  00171100
     1 IARGS( 2 ) .LE. NRC ) GO TO 50                                   00171200
      K = 3                                                             00171300
      GO TO 10                                                          00171400
  50  NROW = IARGS( 1 )                                                 00171500
      NCOL = IARGS( 2 )                                                 00171600
      NRMAX = MIN0( NROW, NRMAX )                                       00171700
      GO TO 20                                                          00171800
      END                                                               00171900
C  25   4      SUBROUTINE DUMMY                    2 19 68              00172000
      SUBROUTINE DUMMY                                                  00172100
      CALL X( 5HDUMMY )                                                 00172200
      RETURN                                                            00172300
      END                                                               00172400
C  26  27      SUBROUTINE ERASE                    2 19 68              00172500
      SUBROUTINE ERASE                                                  00172600
C                                                                       00172700
C ERASE COL ++,++, ++, ETC.                                             00172800
C                                                                       00172900
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00173000
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00173100
      DIMENSION ARGS(100)                                               00173200
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00173300
      IF( NARGS .EQ. 0 ) GO TO 50                                       00173400
      CALL CHKCOL( I )                                                  00173500
      IF( I .EQ. 0 ) GO TO 30                                           00173600
      I = 20                                                            00173700
  10  CALL ERROR( I )                                                   00173800
  20  RETURN                                                            00173900
  30  IF( NERROR .NE. 0 .OR. NRMAX .EQ. 0 ) GO TO 20                    00174000
      DO 40 I = 1, NARGS                                                00174100
  40  CALL VECTOR( 0., IARGS( I ) )                                     00174200
      GO TO 20                                                          00174300
C                                                                       00174400
C     CLEAR ALL OF DIMENSIONED WORKSHEET.                               00174500
C                                                                       00174600
  50  IF( NERROR .NE. 0 ) GO TO 20                                      00174700
      NRMAX = NROW * NCOL                                               00174800
      CALL VECTOR( 0., 1 )                                              00174900
      NRMAX = 0                                                         00175000
      GO TO 20                                                          00175100
      END                                                               00175200
C  27 199      SUBROUTINE ERROR(I)                 2 19 68              00175300
      SUBROUTINE ERROR(I)                                               00175400
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00175500
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00175600
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00175700
      DIMENSION ARGS(100)                                               00175800
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00175900
      COMMON / BLOCKX / INDEX( 6, 8 ), LEVEL                            00176000
      COMMON / SPRV / NERCON,NERR                                       00176100
      DIMENSION IL(2)                                                   00176200
      DATA INCERR, IBL, IL(1),IL(2)  / 1 , 2H  , 2H/I, 2H/F   /         00176300
      ISCRUN=ISCRAT                                                     00176400
C                                                                       00176500
C     IF I IS NEGATIVE, SET INCERR = 1, SO THAT ERRORS WILL BE TALLIED. 00176600
C     IF I IS ZERO, SET INCERR = 0 AND CLEAR NERROR TO ZERO.  ERRORS    00176700
C         WILL NOT BE TALLIED.  (THEY ARE FORGIVEN).                    00176800
C     IF   1 .LE. I .LE. 100, FATAL ERROR                               00176900
C     IF 101 .LE. I .LE. 200, ARITHMETIC ERROR                          00177000
C     IF 201 .LE. I .LE.      INFORMATIVE DIAGNOSTIC                    00177100
C                                                                       00177200
      NERR = NERR + 1                                                   00177300
      IF( I ) 7000, 7001, 7003                                          00177400
 7000 INCERR = 1                                                        00177500
      GO TO 7002                                                        00177600
 7001 INCERR = 0                                                        00177700
      NERROR = 0                                                        00177800
 7002 RETURN                                                            00177900
 7003 IF( I .GT. 100 ) GO TO 200                                        00178000
      NERROR = NERROR + INCERR                                          00178100
      WRITE(ISCRUN,8000)                                                00178200
 8000 FORMAT(84X)                                                       00178300
      WRITE(ISCRUN,8001)                                                00178400
 8001 FORMAT(15H*** FATAL ERROR,69X)                                    00178500
      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,  00178600
     1 23,24,25,26,27,28,29,30), I                                      00178700
   1  WRITE(ISCRUN,801)                                                 00178800
 801  FORMAT(29H*** NAME NOT FOUND IN LIBRARY,55X)                      00178900
      GO TO 900                                                         00179000
   2  WRITE(ISCRUN,802)                                                 00179100
 802  FORMAT(28H*** ILLEGAL STATEMENT NUMBER,56X)                       00179200
      GO TO 888                                                         00179300
   3  WRITE(ISCRUN,803)                                                 00179400
 803  FORMAT(28H*** ILLEGAL ARGUMENT ON CARD,56X)                       00179500
      GO TO 888                                                         00179600
   4  GO TO 900                                                         00179700
   5  WRITE(ISCRUN,805)                                                 00179800
 805  FORMAT(38H*** COMMAND NOT ALLOWED IN REPEAT MODE,46X)             00179900
      GO TO 900                                                         00180000
   6  WRITE(ISCRUN,806)                                                 00180100
 806  FORMAT(54H*** NUMBER MAY NOT BEGIN CARD BETWEEN BEGIN AND FINISH, 00180200
     130X)                                                              00180300
      GO TO 900                                                         00180400
   7  WRITE(ISCRUN,807)                                                 00180500
 807  FORMAT(23H*** ILLEGAL *STATEMENT*,61X)                            00180600
      GO TO 900                                                         00180700
   8  WRITE(ISCRUN,808)                                                 00180800
 808  FORMAT(34H*** PHYSICAL CONSTANT NOT IN TABLE,50X)                 00180900
      GO TO 900                                                         00181000
   9  WRITE(ISCRUN,809)                                                 00181100
 809  FORMAT(13H*** NRMAX = 0,71X)                                      00181200
      GO TO 900                                                         00181300
  10  WRITE(ISCRUN,810)                                                 00181400
 810  FORMAT(31H*** ILLEGAL NUMBER OF ARGUMENTS,53X)                    00181500
      GO TO 888                                                         00181600
  11  WRITE(ISCRUN,811)                                                 00181700
 811  FORMAT(40H*** COLUMN NUMBER TOO BIG OR LESS THAN 1,44X)           00181800
      GO TO 888                                                         00181900
  12  WRITE(ISCRUN,812)                                                 00182000
 812  FORMAT(33H*** COMMAND STORAGE AREA OVERFLOW,51X)                  00182100
      GO TO 900                                                         00182200
  13  WRITE(ISCRUN,813)                                                 00182300
 813  FORMAT(30H*** STATEMENT NUMBER NOT FOUND,54X)                     00182400
      GO TO 888                                                         00182500
  14  WRITE(ISCRUN,814)                                                 00182600
 814  FORMAT(35H*** ILLEGAL OR NO FORMAT DESIGNATOR,49X)                00182700
      GO TO 900                                                         00182800
  15  WRITE(ISCRUN,815)                                                 00182900
 815  FORMAT(34H*** DIMENSIONED AREA EXCEEDS LIMIT,50X)                 00183000
      GO TO 888                                                         00183100
  16  WRITE(ISCRUN,816)                                                 00183200
 816  FORMAT(27H*** ILLEGAL SIZE ROW NUMBER,57X)                        00183300
      GO TO 888                                                         00183400
  17  WRITE(ISCRUN,817)                                                 00183500
 817  FORMAT(39H*** DEFINED MATRIX OVERFLOWS WORKSHEET,45X)             00183600
      GO TO 888                                                         00183700
  18  WRITE(ISCRUN,818)                                                 00183800
 818  FORMAT(36H*** INTEGER ARGUMENT LESS THAN -8191,48X)               00183900
      GO TO 888                                                         00184000
  19  WRITE(ISCRUN,819)                                                 00184100
 819  FORMAT(48H*** STORED PERFORM STATEMENT WILL EXECUTE ITSELF,36X)   00184200
      GO TO 900                                                         00184300
  20  WRITE(ISCRUN,820)                                                 00184400
 820  FORMAT(29H*** IMPROPER TYPE OF ARGUMENT,55X)                      00184500
      GO TO 888                                                         00184600
  21  WRITE(ISCRUN,821)                                                 00184700
 821  FORMAT(26H*** COMMAND MUST BE STORED,58X)                         00184800
      GO TO 900                                                         00184900
  22  WRITE(ISCRUN,822)                                                 00185000
 822  FORMAT(31H*** MATRIX IS (NEARLY) SINGULAR,53X)                    00185100
      GO TO 900                                                         00185200
  23  WRITE(ISCRUN,823)                                                 00185300
 823  FORMAT(33H*** MATRIX IS TOO LARGE TO INVERT,51X)                  00185400
      GO TO 900                                                         00185500
  24  CONTINUE                                                          00185600
  25  CONTINUE                                                          00185700
  26  CONTINUE                                                          00185800
  27  CONTINUE                                                          00185900
  28  CONTINUE                                                          00186000
  29  CONTINUE                                                          00186100
  30  GO TO 900                                                         00186200
  888 IF( LEVEL .EQ. 0 ) GO TO 900                                      00186300
      NRG = MIN0( NARGS , 10 )                                          00186400
      DO 890 II = 1,8                                                   00186500
  890 KIND( II + 89 ) = IBL                                             00186600
      IF( NARGS .EQ. 0 ) GO TO 894                                      00186700
      DO 892 II = 1,NRG                                                 00186800
      J = KIND( II )                                                    00186900
      IF( J .EQ. 0 ) ARGS( II ) = IARGS( II )                           00187000
  892 KIND( II + 89 ) = IL( J+1 )                                       00187100
  894 WRITE(ISCRUN,896) NARGS                                           00187200
  896 FORMAT(4X,I6,70H ARGUMENTS IN COMMAND. THE FIRST  8 ENTRIES IN THE00187300
     1 ARGUMENT POOL ARE..,4X)                                          00187400
      WRITE(ISCRUN,897) ( ARGS( II ) , KIND( II+89 ), II = 1,8 )        00187500
  897 FORMAT(4X,8(F8.2,A2))                                             00187600
 900  IF( LEVEL .NE. 0 ) CALL RNDOWN                                    00187700
C     FORCE OUT OF REPEAT MODE IF FATAL ERROR                           00187800
      IF( I .LE. 100 ) LEVEL = 0                                        00187900
      WRITE( ISCRUN, 901 )                                              00188000
 901  FORMAT(84X)                                                       00188100
      RETURN                                                            00188200
  200 IF( NERR.LE.NERCON) GO TO 201                                     00188300
      WRITE(ISCRUN,9999) NERCON                                         00188400
 9999 FORMAT(1H*,I5,58H INFORMATIVE AND ARITHMETIC DIAGNOSTICS HAVE BEEN00188500
     1 PRINTED.,20X)                                                    00188600
      WRITE(ISCRUN,9998)                                                00188700
 9998 FORMAT(61H* THE PRINTING OF ANY SUCH ADDITIONAL DIAGNOSTICS IS DEL00188800
     1ETED.     ,23X  )                                                 00188900
      RETURN                                                            00189000
  201 IF( I .GT. 200 ) GO TO 400                                        00189100
C                                                                       00189200
C                                                                       00189300
C     ARITHMETIC TROUBLES, SET FLAGS                                    00189400
C                                                                       00189500
      CALL AERR(I-100)                                                  00189600
  250 RETURN                                                            00189700
C                                                                       00189800
C     INFORMATIVE DIAGNOSTIC                                            00189900
C                                                                       00190000
  400 IF( MOD( LLIST , 2 ) .EQ. 0 ) GO TO 250                           00190100
      WRITE(ISCRUN, 901)                                                00190200
      WRITE(ISCRUN, 490)                                                00190300
 490  FORMAT(24H* INFORMATIVE DIAGNOSTIC,60X)                           00190400
      II=I-200                                                          00190500
      GO TO (401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412,00190600
     1 413, 414, 415 ), II                                              00190700
 401  WRITE(ISCRUN,501)                                                 00190800
 501  FORMAT(52H* TOO MUCH DATA IN SET, READ OR GENERATE, SPILL LOST,   00190900
     132X)                                                              00191000
      GO TO 900                                                         00191100
 402  WRITE(ISCRUN,502)                                                 00191200
 502  FORMAT(61H* COMMAND NOT ALLOWED IN REPEAT MODE. EXECUTED BUT NOT S00191300
     1TORED,23X)                                                        00191400
      GO TO 900                                                         00191500
 403  WRITE( ISCRUN, 503 )                                              00191600
 503  FORMAT(38H* VALUE REQUESTED IN SHORTEN NOT FOUND,46X)             00191700
      GO TO 900                                                         00191800
 404  WRITE( ISCRUN, 504 )                                              00191900
 504  FORMAT(32H* BAD HEAD. COLUMN GT 50 OR NO /,52X)                   00192000
      GO TO 900                                                         00192100
 405  WRITE( ISCRUN, 505 )                                              00192200
 505  FORMAT(68H* THIS COMMAND WAS NOT EXECUTED BECAUSE ITS MEANING WAS 00192300
     1QUESTIONABLE,16X)                                                 00192400
      GO TO 900                                                         00192500
 406  WRITE(ISCRUN,506)                                                 00192600
 506  FORMAT(24H* F LESS THAT 0, SET = 0,60X)                           00192700
      GO TO 900                                                         00192800
 407  WRITE(ISCRUN,507)                                                 00192900
 507  FORMAT(24H* NU1 OR NU2 LESS THAN 1,60X)                           00193000
      GO TO 900                                                         00193100
 408  WRITE(ISCRUN,508)                                                 00193200
 508  FORMAT(33H* NU1 OR NU2 TRUNCATED TO INTEGER,51X)                  00193300
      GO TO 900                                                         00193400
 409  WRITE(ISCRUN,509)                                                 00193500
 509  FORMAT(34H* IMPROPER TITLE NUMBER, ASSUMED 1,50X)                 00193600
      GO TO 900                                                         00193700
 410  WRITE(ISCRUN,510)                                                 00193800
 510  FORMAT(36H* COLUMN TOO SHORT FOR ENTIRE MATRIX,48X)               00193900
      GO TO 900                                                         00194000
 411  WRITE(ISCRUN,511)                                                 00194100
 511  FORMAT(52H* ASTERISK STRING IMPLYING "THRU" INCORRECT, IGNORED,   00194200
     1 32X)                                                             00194300
      GO TO 900                                                         00194400
 412  WRITE(ISCRUN,512)                                                 00194500
 512  FORMAT(34H* LAST ARGUMENT IN COMMAND IGNORED,50X)                 00194600
      GO TO 900                                                         00194700
 413  WRITE(ISCRUN,513)                                                 00194800
 513  FORMAT(50H* DEMOTE WENT PAST BOTTOM OF WORKSHEET, SPILL LOST,34X) 00194900
 414  CONTINUE                                                          00195000
 415  CONTINUE                                                          00195100
      GO TO 900                                                         00195200
      END                                                               00195300
C  28  32      SUBROUTINE EXCHNG                   2 19 68              00195400
      SUBROUTINE EXCHNG                                                 00195500
C                                                                       00195600
C EXCHANGE COL ++ WITH ++, COL ++ WITH ++, ETC.                         00195700
C                                                                       00195800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00195900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00196000
      DIMENSION ARGS(100)                                               00196100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00196200
      IF (NARGS) 910,910,5                                              00196300
    5 IF (NARGS.NE.(NARGS/2)*2) GO TO 910                               00196400
      DO 30 I=1,NARGS,2                                                 00196500
      CALL ADRESS( I, J )                                               00196600
      IF (J) 903,911,10                                                 00196700
  10  CALL ADRESS( I+1, K )                                             00196800
      IF (K) 903,911,11                                                 00196900
   11 IF (NERROR.NE.0) RETURN                                           00197000
      DO 20 N=1,NRMAX                                                   00197100
      JJ=J+N-1                                                          00197200
      KK=K+N-1                                                          00197300
      WORK=RC(JJ)                                                       00197400
      RC(JJ)=RC(KK)                                                     00197500
      RC(KK)=WORK                                                       00197600
   20 CONTINUE                                                          00197700
   30 CONTINUE                                                          00197800
      GO TO 999                                                         00197900
  903 CALL ERROR (3)                                                    00198000
      GO TO 999                                                         00198100
  910 CALL ERROR (10)                                                   00198200
      GO TO 999                                                         00198300
  911 CALL ERROR (11)                                                   00198400
  999 RETURN                                                            00198500
      END                                                               00198600
C  29  80      SUBROUTINE EXPAND( J, WHERE )       2 19 68              00198700
      SUBROUTINE EXPAND( J, WHERE )                                     00198800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00198900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00199000
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00199100
      DIMENSION ARGS(100)                                               00199200
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00199300
      DIMENSION WHERE( 1 )                                              00199400
C                                                                       00199500
C         THIS ROUTINE EXPANDS STORED COMMANDS FROM WHERE TO A USABLE   00199600
C         FORM IN ARGS, IARGS AND KIND.                                 00199700
C                                                                       00199800
      II = 0                                                            00199900
      I  = 0                                                            00200000
      JJJ = J                                                           00200100
C                                                                       00200200
C     CONVERT ONLY FIRST ARGUMENT IF COMMAND IS INCREMENT OR RESTORE    00200300
C                                                                       00200400
      IF( L1 .NE. 14 ) GO TO 10                                         00200500
      IF( L2 .GE. 6 .AND. L2 .LE. 8 ) JJJ = 2                           00200600
   10 II = II + 1                                                       00200700
   15 I = I + 1                                                         00200800
      IF( I .GE. JJJ ) GO TO 45                                         00200900
      T= WHERE( I )                                                     00201000
      IF( T ) 40, 30, 20                                                00201100
   20 KIND( II ) = 0                                                    00201200
      IARGS( II ) = T - 8192.                                           00201300
      GO TO 10                                                          00201400
   30 KIND( II ) = 1                                                    00201500
      I = I + 1                                                         00201600
      ARGS( II ) = WHERE( I )                                           00201700
      GO TO 10                                                          00201800
   40 IF( T .EQ. -1. ) GO TO 100                                        00201900
      CALL XPND( WHERE( I ), K ,ARGS( II ) , KND )                      00202000
      IF( K .GE. 0 ) GO TO 50                                           00202100
      K = - K                                                           00202200
   42 CALL ERROR( K )                                                   00202300
   45 RETURN                                                            00202400
   50 KIND( II ) = KND                                                  00202500
      IF( KND .EQ. 0 ) IARGS( II ) = ARGS( II )                         00202600
      I = I + K                                                         00202700
      GO TO 10                                                          00202800
C                                                                       00202900
C     IF STORED VALUE = -1, THEN ARGS (INTEGER) ARE TO BE EXPANDED FROM 00203000
C     PREVIOUS ARG TO FOLLOWING WITH A MAXIMUM TOTAL OF 50              00203100
C                                                                       00203200
  100 I = I + 1                                                         00203300
C     PICK UP NEXT ARG                                                  00203400
      IU = WHERE( I )                                                   00203500
      IF( KIND( II-1 ) .NE. 0 .OR. I .GE. J ) GO TO 200                 00203600
      IF( IU ) 160, 200, 105                                            00203700
  105 IU =IU - 8192                                                     00203800
      K= IU - IARGS( II-1 )                                             00203900
      NARGS = NARGS + IABS( K ) - 1                                     00204000
      IF ( NARGS .GT. 50 ) GO TO 250                                    00204100
      IF ( K ) 110,15,120                                               00204200
  110 INC = -1                                                          00204300
      K = -K                                                            00204400
      GO TO 140                                                         00204500
  120 INC = 1                                                           00204600
  140 DO 150 IT = 1, K                                                  00204700
      KIND( II ) = 0                                                    00204800
      IARGS( II ) = IARGS( II-1 ) + INC                                 00204900
  150 II = II + 1                                                       00205000
      GO TO 15                                                          00205100
C                                                                       00205200
C     EXPAND FORM  IARG *** "" ARG ""                                   00205300
C                                                                       00205400
  160 CALL XPND( WHERE( I ) , K , ARGS( II ) , KND )                    00205500
      IF( K .LT. 0 ) GO TO 42                                           00205600
      I = I + K                                                         00205700
      IF( KND .EQ. 0 ) GO TO 170                                        00205800
      K = 20                                                            00205900
      GO TO 42                                                          00206000
  170 IU = ARGS( II )                                                   00206100
      GO TO 105                                                         00206200
  200 CALL ERROR( 211 )                                                 00206300
      GO TO 10                                                          00206400
  250 K= 10                                                             00206500
      GO TO 42                                                          00206600
      END                                                               00206700
C  30  97      SUBROUTINE EXPCON                   2 19 68              00206800
      SUBROUTINE EXPCON                                                 00206900
C**** EXPCON  SUBROUTINE    S PEAVY   9/12/67                           00207000
C**** COMMANDS                                                          00207100
C****     L2=1" MVECDIAG,AVECDIAG                                       00207200
C****         MVECDIAG  A(,, ++) R=,, C=++ STORE IN COL ++              00207300
C****      OR MVECDIAG  A(,, ++) R=,, C=++ START STORING IN (,, ++)     00207400
C****     L2=2" MVECMAT,AVECTARR                                        00207500
C****         MVECMAT A(,, ++) R=,, C=++ STORE IN COL ++                00207600
C****      OR MVECMAT A(,, ++) R=,, C=++ START STORING IN (,, ++)       00207700
C****     THE MATRIX A IS STORED BY ROWS AS A COL VECTOR                00207800
C****     L2=3" MMATVEC                                                 00207900
C****         MMATVEC COL +++    STORE IN A(,, ++) AS R=,, BY C=++ MATRI00208000
C****      OR MMATVEC STARTING (,, ++) IN A(,, ++) AS R=,, BY C=++ MATRI00208100
C****     THIS OPERATION IS THE REVERSE OF MVECMAT                      00208200
      COMMON / SCRAT / A(10000),NS                                      00208300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00208400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00208500
      DIMENSION ARGS(100)                                               00208600
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00208700
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00208800
      IF(NARGS.LT.5.OR.NARGS.GT.6)  CALL ERROR (10)                     00208900
      IF(L2.EQ.3) GO TO 300                                             00209000
  100 J=NARGS                                                           00209100
      CALL CKIND(J)                                                     00209200
      IF(J.NE.0)  CALL ERROR(3)                                         00209300
      J=1                                                               00209400
      CALL MTXCHK(J)                                                    00209500
      IF(J.NE.0) CALL ERROR(17)                                         00209600
      CALL ADRESS(NARGS,ILL)                                            00209700
      IF(ILL.LE.0) CALL ERROR(11)                                       00209800
      IM=IARGS(1)                                                       00209900
      ILC=ILL                                                           00210000
      IL=IARGS(3)                                                       00210100
      IF(L2.GT.1) IL=IL*IARGS(4)                                        00210200
      IF(NARGS.EQ.6) ILC=ILC+IARGS(5)-1                                 00210300
      IXX=ILC+IL-1                                                      00210400
      IF(L2-2)  105,200,310                                             00210500
C**** VEC DIAG                                                          00210600
  105 IF(IXX.LE.ILL+NROW-1) GO TO 120                                   00210700
      IXX=ILL+NROW-1                                                    00210800
      CALL ERROR( 210 )                                                 00210900
C**** ERROR 220 "COLUMN NOT LONG ENOUGH TO STORE ALL OF DIAGONAL.       00211000
C**** NROW ELEMENTS WILL BE STORED. "                                   00211100
  120 IF(NERROR.NE.0) RETURN                                            00211200
      DO  130  I=ILC,IXX                                                00211300
      RC(I)=RC(IM)                                                      00211400
  130 IM=IM+1+NROW                                                      00211500
      RETURN                                                            00211600
C**** VECTMAT                                                           00211700
  200 IF(IXX.LE.ILL+NROW-1) GO TO 220                                   00211800
      IXX=ILL+NROW-1                                                    00211900
      CALL ERROR( 210 )                                                 00212000
C**** ERROR 221" "COLUMN NOT LONG ENOUGH TO STORE ALL OF MATRIX (ARRAY).00212100
C**** NROW ELEMENTS WILL BE STORED."                                    00212200
  220 IF(NERROR.NE.0) RETURN                                            00212300
      KMX=IARGS(4)                                                      00212400
      NMX=IARGS(3)                                                      00212500
      IMM=IM                                                            00212600
      DO  240  J=1,NMX                                                  00212700
      IM=IMM                                                            00212800
      DO  230  I=1,KMX                                                  00212900
      RC(ILC)=RC(IM)                                                    00213000
      IM=IM+NROW                                                        00213100
      IF(ILC.EQ.IXX)  RETURN                                            00213200
  230 ILC=ILC+1                                                         00213300
  240 IMM=IMM+1                                                         00213400
      RETURN                                                            00213500
C**** RESTORE MATRIX OR ARRAY                                           00213600
  300 IL=IARGS(1)                                                       00213700
      IC=2                                                              00213800
      IF(NARGS.NE.6) GO TO 302                                          00213900
      IC=3                                                              00214000
      ILL=IARGS(2)                                                      00214100
  302 DO 305 I=1,4                                                      00214200
      IARGS(I)=IARGS(IC)                                                00214300
  305 IC=IC+1                                                           00214400
      IARGS( 5)=IL                                                      00214500
      IARGS(6)=ILL                                                      00214600
      GO TO 100                                                         00214700
  310 IF(IXX.LE.ILL+NROW-1) GO TO 320                                   00214800
      IXX=ILL+NROW-1                                                    00214900
      CALL ERROR( 210 )                                                 00215000
C**** ERROR 222""NOT N BY K ELEMENTS IN COLUMN TO RESTORE AS N BY K     00215100
C**** MATRIX (ARRAY). THEREFORE NROW ELEMENTS WILL BE STORED."          00215200
  320 IF(NERROR.NE.0) RETURN                                            00215300
      NMX=IARGS(3)                                                      00215400
      KMX=IARGS(4)                                                      00215500
      DO 340  I=1,NMX                                                   00215600
      IMC=IM                                                            00215700
      DO 330  J=1,KMX                                                   00215800
      RC(IMC)=RC(ILC)                                                   00215900
      IF(ILC.EQ.IXX) RETURN                                             00216000
      IMC=IMC+NROW                                                      00216100
  330 ILC=ILC+1                                                         00216200
  340 IM=IM+1                                                           00216300
      RETURN                                                            00216400
      END                                                               00216500
C  31  49      SUBROUTINE EXTREM                   2 19 68              00216600
      SUBROUTINE EXTREM                                                 00216700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00216800
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00216900
      DIMENSION ARGS(100)                                               00217000
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00217100
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00217200
C                                                                       00217300
C     L2 = 4,5 MAX     L2 = 6,7 MIN                                     00217400
C                                                                       00217500
C     MAX OF ++ TO ++                                                   00217600
C     MAX OF ++ TO ++, CORRESP ENTRY OF ++ TO ++, ++ TO ++, ETC.        00217700
C     LIKEWISE FOR MIN.                                                 00217800
C                                                                       00217900
      IF( NARGS .GT. 0 .AND. MOD( NARGS, 2 ) .EQ. 0 ) GO TO 30          00218000
      I=10                                                              00218100
  10  CALL ERROR( I )                                                   00218200
  20  RETURN                                                            00218300
  30  CALL CHKCOL( I )                                                  00218400
      IF( I .EQ. 0 ) GO TO 40                                           00218500
      I = 20                                                            00218600
      GO TO 10                                                          00218700
  40  IF( NERROR .NE. 0 ) GO TO 20                                      00218800
      J=0                                                               00218900
      IF( NRMAX - 1 ) 50, 110, 60                                       00219000
  50  I=9                                                               00219100
      GO TO 10                                                          00219200
  60  J = IARGS( 1 )                                                    00219300
      K = J + 1                                                         00219400
      L = K + NRMAX - 2                                                 00219500
      IF( L2 .GT. 5 ) GO TO 80                                          00219600
C                                                                       00219700
C     FIND MAXIMUM                                                      00219800
C                                                                       00219900
      DO 70 I = K, L                                                    00220000
      IF( RC( J ) .LT. RC( I ) ) J = I                                  00220100
  70  CONTINUE                                                          00220200
      GO TO 100                                                         00220300
C                                                                       00220400
C     FIND MINIMUM                                                      00220500
C                                                                       00220600
  80  DO 90 I = K, L                                                    00220700
      IF( RC( J ) .GT. RC( I ) ) J = I                                  00220800
  90  CONTINUE                                                          00220900
 100  J = J - IARGS( 1 )                                                00221000
 110  DO 120 I = 1, NARGS, 2                                            00221100
      K = IARGS( I ) + J                                                00221200
      XY=RC(K)                                                          00221300
  120 CALL VECTOR (XY,IARGS(I+1))                                       00221400
      GO TO 20                                                          00221500
      END                                                               00221600
C  32   9      FUNCTION FCOS( X )                  2 19 68              00221700
      FUNCTION FCOS( X )                                                00221800
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00221900
      IF( ABS( X ) .GT. XTRIG ) GO TO 2                                 00222000
      FCOS = COS( X )                                                   00222100
   1  RETURN                                                            00222200
   2  CALL ERROR( 110 )                                                 00222300
      FCOS = 0.                                                         00222400
      GO TO 1                                                           00222500
      END                                                               00222600
C  33   9      FUNCTION FEXP( X )                  2 19 68              00222700
      FUNCTION FEXP( X )                                                00222800
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00222900
      IF( X .GT. XEXP ) GO TO 2                                         00223000
      FEXP = EXP( X )                                                   00223100
   1  RETURN                                                            00223200
   2  CALL ERROR( 102 )                                                 00223300
      FEXP = 0.                                                         00223400
      GO TO 1                                                           00223500
      END                                                               00223600
C  34  12      FUNCTION FEXP2( B, E )              2 19 68              00223700
      FUNCTION FEXP2( B, E )                                            00223800
C                                                                       00223900
C     THIS FUNCTION IS INCLUDED TO CATCH EXPONENTIATION ERRORS BEFORE   00224000
C     THE SYSTEM DOES                                                   00224100
C                                                                       00224200
      IE = E                                                            00224300
      IF( E .EQ. FLOAT( IE ) ) GO TO 2                                  00224400
      FEXP2 = FEXP( E * FLOG( B ) )                                     00224500
   1  RETURN                                                            00224600
   2  FEXP2 = B ** IE                                                   00224700
      GO TO 1                                                           00224800
      END                                                               00224900
C  35   4      SUBROUTINE FFLOAT                   2 19 68              00225000
      SUBROUTINE FFLOAT                                                 00225100
      CALL X( "FFLOAT" )                                                00225200
      RETURN                                                            00225300
      END                                                               00225400
C  36   4      SUBROUTINE FIXED                    2 19 68              00225500
      SUBROUTINE FIXED                                                  00225600
      CALL X( "FIXED" )                                                 00225700
      RETURN                                                            00225800
      END                                                               00225900
C  37  40      SUBROUTINE FIXFLO                   2 19 68              00226000
      SUBROUTINE FIXFLO                                                 00226100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00226200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00226300
      DIMENSION ARGS(100)                                               00226400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00226500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00226600
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00226700
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00226800
      DIMENSION ITYPE( 2 ), N( 11 )                                     00226900
      DATA N(1),N(2),N(3),N(4),N(5),N(6),N(7),N(8),N(9),N(10),N(11) /   00227000
     1 2H0), 2H1), 2H2), 2H3), 2H4), 2H5), 2H6), 2H7), 2H8), 2H9),3H10)/00227100
      DATA ITYPE(1), ITYPE(2) / 2H8F, 4H1P8E /                          00227200
C                                                                       00227300
C     L2 = 3 FOR FIXED, L2 = 4 FOR FLOAT                                00227400
C                                                                       00227500
C     FORMAT IS 4 WORDS       WHEN FLOAT IS EXECUTED 2. BECOMES 1P8E    00227600
C     1. C                    WHEN FIXED IS EXECUTED 2. BECOMES 8F      00227700
C     2. 1P8E                                                           00227800
C     3. 15.                                                            00227900
C     4. 6)                                                             00228000
C                                                                       00228100
C     THIS ROUTINE ASSUMES THAT AT LEAST FOUR CHARACTERS MAY BE STORED  00228200
C     IN A WORD, THAT UNUSED SPACE IN A WORD INITIALIZED WITH CHARACTERS00228300
C     WILL BE FILLED WITH BLANKS, AND THAT BLANKS ARE IGNORED IF FORMAT 00228400
C     SCANNING (EXCEPT IN HOLLERITH FIELDS).                            00228500
C                                                                       00228600
      IF( NARGS .EQ. 1 ) IF( KIND( 1 ) ) 30, 40, 30                     00228700
      I = 10                                                            00228800
  10  CALL ERROR( I )                                                   00228900
  20  RETURN                                                            00229000
  30  I = 20                                                            00229100
      GO TO 10                                                          00229200
  40  I = IARGS( 1 )                                                    00229300
      IF( I .GE. 0 .AND. I .LE. 10 ) GO TO 50                           00229400
      I = 3                                                             00229500
      GO TO 10                                                          00229600
  50  IFMTX( 4 ) = N( I+1 )                                             00229700
      IFMTX( 2 ) = ITYPE( L2-2 )                                        00229800
      GO TO 20                                                          00229900
      END                                                               00230000
C  38  41      SUBROUTINE FLIP                     2 19 68              00230100
      SUBROUTINE FLIP                                                   00230200
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00230300
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00230400
      DIMENSION ARGS(100)                                               00230500
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00230600
      EQUIVALENCE  (I,IARGS(100)),(J,IARGS(99)),(K,IARGS(98)),          00230700
     1 (KK,IARGS(97)),(M,IARGS(96)),(MM,IARGS(95)),(MMM,IARGS(94)),     00230800
     2 (N,IARGS(93)),(NN,IARGS(92)),(A,ARGS(1))                         00230900
C                                                                       00231000
C FLIP COL ++ INTO COL ++, ++ INTO ++, ETC.                             00231100
C                                                                       00231200
      IF( NARGS .GT. 0 .AND. MOD( NARGS, 2 ) .GE. 0 ) GO TO 20          00231300
      I = 10                                                            00231400
  10  CALL ERROR( I )                                                   00231500
  15  RETURN                                                            00231600
  20  CALL CHKCOL( I )                                                  00231700
      IF( I .EQ. 0 ) GO TO 30                                           00231800
      I = 20                                                            00231900
      GO TO 10                                                          00232000
  30  IF( NERROR .NE. 0 ) GO TO 15                                      00232100
      IF( NRMAX - 1 ) 35, 15, 40                                        00232200
  35  I = 9                                                             00232300
      GO TO 10                                                          00232400
  40  KK = NRMAX - 1                                                    00232500
      K = KK / 2                                                        00232600
      DO 60 I = 1, NARGS, 2                                             00232700
      M = IARGS( I )                                                    00232800
      N = IARGS( I+1 )                                                  00232900
      MM = M + KK                                                       00233000
      NN = N + KK                                                       00233100
      MMM = M + K                                                       00233200
      DO 50 J = M, MMM                                                  00233300
      A = RC( J )                                                       00233400
      RC( N ) = RC( MM )                                                00233500
      RC( NN ) = A                                                      00233600
      N = N + 1                                                         00233700
      MM = MM - 1                                                       00233800
  50  NN = NN - 1                                                       00233900
  60  CONTINUE                                                          00234000
      GO TO 15                                                          00234100
      END                                                               00234200
C  39   8      FUNCTION FLOG( X )                  2 19 68              00234300
      FUNCTION FLOG( X )                                                00234400
      IF( X .GT. 0. ) GO TO 1                                           00234500
      CALL ERROR( 101 )                                                 00234600
      FLOG = 0.                                                         00234700
      GO TO 2                                                           00234800
   1  FLOG = ALOG( X )                                                  00234900
   2  RETURN                                                            00235000
      END                                                               00235100
C  40  59      SUBROUTINE  FPROB                   2 19 68              00235200
      SUBROUTINE  FPROB                                                 00235300
C     WRITTEN BY  S PEAVY             10/13/67                          00235400
C     COMMAND IS AS  FOLLOWING                                          00235500
C        FPROBABILITY V1 $$,V2 $$, F  $$, STORE Q IN COL ++             00235600
      COMMON / SCRAT / A(10000),NS                                      00235700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00235800
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00235900
      DIMENSION ARGS(100)                                               00236000
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00236100
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00236200
      IF (NARGS.NE.4) CALL  ERROR(10)                                   00236300
      IF (KIND(NARGS).NE.0) CALL ERROR (3)                              00236400
      I1=1                                                              00236500
      I2=1                                                              00236600
      I3=1                                                              00236700
  150 CALL ADRESS (1,IARGS(1))                                          00236800
      IF(IARGS(1)) 160,170,175                                          00236900
  160 I1=2                                                              00237000
      V1=ARGS(1)                                                        00237100
      GO TO 180                                                         00237200
  170 CALL ERROR (11)                                                   00237300
  175 L=IARGS(1)                                                        00237400
  180 CALL ADRESS (2,IARGS(2))                                          00237500
      IF(IARGS(2)) 190,200,205                                          00237600
  190 I2=2                                                              00237700
      V2=ARGS(2)                                                        00237800
      GO TO 210                                                         00237900
  200 CALL ERROR(11)                                                    00238000
  205 M=IARGS(2)                                                        00238100
  210 CALL ADRESS(3,IARGS(3))                                           00238200
      IF(IARGS(3)) 220,230,235                                          00238300
  220 I3=2                                                              00238400
      F=ARGS(3)                                                         00238500
      GO TO 240                                                         00238600
  230 CALL ERROR(11)                                                    00238700
  235 N=IARGS(3)                                                        00238800
  240 CALL ADRESS(NARGS,K)                                              00238900
      IF(K.LE.0) CALL ERROR (11)                                        00239000
      IF(NERROR.NE.0) RETURN                                            00239100
      IF(I1+I2+I3.NE.6) GO TO 260                                       00239200
      CALL PROB(V1,V2,F,Q)                                              00239300
      DO  250 I=1,NRMAX                                                 00239400
      RC(K)=Q                                                           00239500
  250 K=K+1                                                             00239600
      RETURN                                                            00239700
  260 DO 330  I=1,NRMAX                                                 00239800
      GO TO (270,280),I1                                                00239900
  270 V1=RC(L)                                                          00240000
      L=L+1                                                             00240100
  280 GO TO (290,300),I2                                                00240200
  290 V2=RC(M)                                                          00240300
      M=M+1                                                             00240400
  300 GO TO (310,320),I3                                                00240500
  310 F=RC(N)                                                           00240600
      N=N+1                                                             00240700
  320 CALL PROB(V1,V2,F,RC(K))                                          00240800
  330 K=K+1                                                             00240900
      RETURN                                                            00241000
      END                                                               00241100
C  41  38      SUBROUTINE FOURIA                   2 19 68              00241200
      SUBROUTINE FOURIA                                                 00241300
      COMMON /BEZON/Y(200),A,R(199),       N,KAA                        00241400
      DOUBLE PRECISION  Y,A,R,AA,AB,   AC,BA,BB,AD                      00241500
      M=N/2                                                             00241600
      K=2*M                                                             00241700
      L=0                                                               00241800
      IF (N.EQ.K) GO TO 2                                               00241900
      L=1                                                               00242000
    2 AB=N                                                              00242100
      AA=6.28318530717D0/AB                                             00242200
      A=0.0                                                             00242300
      R(M)=.0                                                           00242400
      AC=1.                                                             00242500
      DO 3 I=1,N                                                        00242600
      A=A+Y(I)                                                          00242700
      R(M)=R(M)+AC*Y(I)                                                 00242800
    3 AC=-1.*AC                                                         00242900
      A=A/AB                                                            00243000
      R(M)=R(M)/AB                                                      00243100
      J=M+L-1                                                           00243200
      KA=M+1                                                            00243300
      DO 5 K=1,J                                                        00243400
      BA=Y(1)                                                           00243500
      BB=0.0                                                            00243600
      AC=K                                                              00243700
      AC=AC*AA                                                          00243800
      DO 4 I=2,N                                                        00243900
      AD=I-1                                                            00244000
      AD=AD*AC                                                          00244100
      BA=BA+Y(I)*DCOS(AD)                                               00244200
    4 BB=BB+Y(I)*DSIN(AD)                                               00244300
      R(K)=2.*BA/AB                                                     00244400
      R(KA)=2.*BB/AB                                                    00244500
  5   KA=KA+1                                                           00244600
      IF (L.EQ.1) GO TO 6                                               00244700
      R(KA)=0.                                                          00244800
    6 RETURN                                                            00244900
      END                                                               00245000
C  42   9      FUNCTION FSIN( X )                  2 19 68              00245100
      FUNCTION FSIN( X )                                                00245200
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00245300
      IF( ABS( X ) .GT. XTRIG ) GO TO 2                                 00245400
      FSIN = SIN( X )                                                   00245500
   1  RETURN                                                            00245600
   2  CALL ERROR( 110 )                                                 00245700
      FSIN = 0.                                                         00245800
      GO TO 1                                                           00245900
      END                                                               00246000
C  43   8      FUNCTION FSQRT( X )                 2 19 68              00246100
      FUNCTION FSQRT( X )                                               00246200
      IF( X .LT. 0. ) GO TO 2                                           00246300
      FSQRT = SQRT( X )                                                 00246400
   1  RETURN                                                            00246500
   2  CALL ERROR( 101 )                                                 00246600
      FSQRT = 0.                                                        00246700
      GO TO 1                                                           00246800
      END                                                               00246900
C  44 207      SUBROUTINE FUNCT                    2 19 68              00247000
      SUBROUTINE FUNCT                                                  00247100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00247200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00247300
      DIMENSION ARGS(100)                                               00247400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00247500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00247600
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00247700
      DIMENSION II( 2 )                                                 00247800
      EQUIVALENCE ( I1, II( 1 ) ), ( I2, II( 2 ) )                      00247900
C                                                                       00248000
C         THIS SUBROUTINE HANDLES ALL TWO AND THREE ARGUMENT FUNCTIONS. 00248100
C     IF THE FIRST ARGUMENT IS A CONSTANT, THE FUNCTION IS EVALUATED    00248200
C         ONLY ONCE.                                                    00248300
C                                                                       00248400
      IF( NARGS .EQ. 2 .OR. NARGS .EQ. 3 ) GO TO 10                     00248500
      CALL ERROR( 10 )                                                  00248600
      GO TO 200                                                         00248700
  10  CALL ADRESS( NARGS, IL )                                          00248800
      IF( IL ) 20, 30, 40                                               00248900
  20  CALL ERROR( 20 )                                                  00249000
      GO TO 200                                                         00249100
  30  CALL ERROR( 11 )                                                  00249200
      GO TO 200                                                         00249300
  40  ILZ = IL + NRMAX - 1                                              00249400
      NARGS = NARGS - 1                                                 00249500
      DO 50 I = 1, NARGS                                                00249600
      CALL ADRESS( I, II( I ) )                                         00249700
      IF( II( I ) ) 45, 30, 50                                          00249800
  45  II( I ) = -II( I )                                                00249900
  50  CONTINUE                                                          00250000
      IF( KIND( 1 ) .EQ. 0 ) GO TO 55                                   00250100
      X = ARGS( 1 )                                                     00250200
      LOCRTN = 1                                                        00250300
      GO TO 250                                                         00250400
  52  ARGS( 1 ) = X                                                     00250500
  55  IF( NERROR .NE. 0 ) GO TO 200                                     00250600
      IF( NRMAX .NE. 0 ) GO TO 60                                       00250700
      CALL ERROR( 9 )                                                   00250800
      GO TO 200                                                         00250900
  60  IF( NARGS .EQ. 2 ) GO TO 90                                       00251000
      IF( KIND( 1 ) .EQ. 0 ) GO TO 70                                   00251100
C                                                                       00251200
C         TWO ARGUMENTS, FIRST IS A CONSTANT                            00251300
C                                                                       00251400
      CALL VECTOR( ARGS( 1 ), IL )                                      00251500
      GO TO 200                                                         00251600
C                                                                       00251700
C         TWO ARGUMENTS, FIRST IS A COLUMN NUMBER                       00251800
C                                                                       00251900
  70  LOCRTN = 2                                                        00252000
      I = IL                                                            00252100
   80 IF ( I .GT.ILZ ) GO TO 200                                        00252200
      X = RC( I1 )                                                      00252300
      GO TO 250                                                         00252400
  75  RC( I ) = X                                                       00252500
      I1 = I1 + 1                                                       00252600
      I=I+1                                                             00252700
      GO TO 80                                                          00252800
  90  K2 = 1 - KIND( 2 )                                                00252900
      IF( KIND( 1 ) .EQ. 0 ) GO TO 110                                  00253000
C                                                                       00253100
C         THREE ARGUMENTS, FIRST ONE A CONSTANT                         00253200
C                                                                       00253300
      DO 100 I = IL, ILZ                                                00253400
      RC( I ) = RC( I ) + RC( I2 ) * ARGS( 1 )                          00253500
 100  I2 = I2 + K2                                                      00253600
      GO TO 200                                                         00253700
C                                                                       00253800
C         THREE ARGUMENTS, FIRST A COLUMN NUMBER                        00253900
C                                                                       00254000
 110  LOCRTN = 3                                                        00254100
      I=IL                                                              00254200
  120 IF ( I .GT.ILZ ) GO TO 200                                        00254300
      X = RC( I1 )                                                      00254400
      GO TO 250                                                         00254500
 115  RC( I ) = RC( I ) + RC( I2 ) * X                                  00254600
      I1 = I1 + 1                                                       00254700
      I2 = I2 + K2                                                      00254800
      I=I+1                                                             00254900
      GO TO 120                                                         00255000
 200  RETURN                                                            00255100
 250  GO TO ( 300,310,320,330,340,350,360,370,380,390,400,410,420,430,  00255200
     1 440,450,460,460,470,480,480,490,500,500,510,520,530,540,550,560, 00255300
     2 570,580,590,600,610,620,630 ) , L2                               00255400
 260  CALL ERROR( L )                                                   00255500
      X = 0.                                                            00255600
 275  GO TO ( 52, 75, 115 ), LOCRTN                                     00255700
C     SIN                                                               00255800
 300  X = FSIN( X )                                                     00255900
      GO TO 275                                                         00256000
C     COS                                                               00256100
 310  X = FCOS( X )                                                     00256200
      GO TO 275                                                         00256300
C     TAN                                                               00256400
 320  X = FSIN( X ) / FCOS( X )                                         00256500
      GO TO 275                                                         00256600
C     COT                                                               00256700
 330  X = FCOS( X ) / FSIN( X )                                         00256800
      GO TO 275                                                         00256900
C     ASIN                                                              00257000
 340  IF( ABS( X ) - 1. ) 341, 342, 343                                 00257100
 341  X = ATAN( X / SQRT( 1. - X ** 2 ) )                               00257200
      GO TO 275                                                         00257300
 342  X = SIGN( HALFPI, X )                                             00257400
      GO TO 275                                                         00257500
 343  L = 103                                                           00257600
      GO TO 260                                                         00257700
C     ACOS                                                              00257800
 350  IF( ABS( X ) - 1. ) 351, 351, 343                                 00257900
 351  X = ATAN( SQRT( 1. - X ** 2 ) / X )                               00258000
      GO TO 275                                                         00258100
C     ATAN                                                              00258200
 360  X = ATAN( X )                                                     00258300
      GO TO 275                                                         00258400
C     ACOT                                                              00258500
 370  X = ATAN( 1. / X )                                                00258600
      GO TO 275                                                         00258700
C     SIND                                                              00258800
  380 X = RAD * X                                                       00258900
      GO TO 300                                                         00259000
C     COSD                                                              00259100
  390 X = RAD * X                                                       00259200
      GO TO 310                                                         00259300
C     TAND                                                              00259400
  400 X = RAD * X                                                       00259500
      GO TO 320                                                         00259600
C     COTD                                                              00259700
  410 X = RAD * X                                                       00259800
      GO TO 330                                                         00259900
C     ASIND                                                             00260000
 420  IF( ABS( X ) - 1. ) 421, 422, 343                                 00260100
  421 X = DEG * ATAN(X/SQRT(1. - X ** 2 ) )                             00260200
      GO TO 275                                                         00260300
 422  X = SIGN( 90., X )                                                00260400
      GO TO 275                                                         00260500
C     ACOSD                                                             00260600
 430  IF( ABS( X ) - 1. ) 431, 431, 343                                 00260700
  431 X = DEG * ATAN(SQRT( 1. - X ** 2 ) / X)                           00260800
      GO TO 275                                                         00260900
C     ATAND                                                             00261000
  440 X = DEG * ATAN(X)                                                 00261100
      GO TO 275                                                         00261200
C     ACOTD                                                             00261300
  450 X = DEG * ATAN( 1. / X)                                           00261400
      GO TO 275                                                         00261500
C     ABS                                                               00261600
 460  X = ABS( X )                                                      00261700
      GO TO 275                                                         00261800
C     SQRT                                                              00261900
 470  X = FSQRT( X )                                                    00262000
      GO TO 275                                                         00262100
C     EXP                                                               00262200
 480  X = FEXP( X )                                                     00262300
      GO TO 275                                                         00262400
C     NEXP                                                              00262500
 490  X = FEXP( -X )                                                    00262600
      GO TO 275                                                         00262700
C     LOG                                                               00262800
 500  X = FLOG( X )                                                     00262900
      GO TO 275                                                         00263000
C     LOG10                                                             00263100
 510  IF( X .GT. 0. ) GO TO 511                                         00263200
      L = 101                                                           00263300
      GO TO 260                                                         00263400
 511  X = ALOG10( X )                                                   00263500
      GO TO 275                                                         00263600
C     ALOG                                                              00263700
 520  IF( X .GT. XALOG ) GO TO 522                                      00263800
      X = 10. ** X                                                      00263900
      GO TO 275                                                         00264000
 522  L = 102                                                           00264100
      GO TO 260                                                         00264200
C     SINH                                                              00264300
 530  Y = FEXP( X )                                                     00264400
      X = .5 * ( Y + 1. / Y ) * TANH( X )                               00264500
      GO TO 275                                                         00264600
C     COSH                                                              00264700
 540  Y = FEXP( X )                                                     00264800
      X = .5 * ( Y + 1. / Y )                                           00264900
      GO TO 275                                                         00265000
C     TANH                                                              00265100
 550  X = TANH( X )                                                     00265200
      GO TO 275                                                         00265300
C     COTH                                                              00265400
 560  X = 1. / TANH( X )                                                00265500
      GO TO  275                                                        00265600
C     ASINH                                                             00265700
 570  X = SIGN( ALOG( ABS( X ) + SQRT( X ** 2 + 1. ) ), X )             00265800
      GO TO 275                                                         00265900
C     ACOTH                                                             00266000
 580  X = ALOG( ABS( X ) + SQRT( X ** 2 - 1. ) )                        00266100
      GO TO 275                                                         00266200
C     ATANH                                                             00266300
 590  IF( ABS( X ) .LT. 1. ) GO TO 592                                  00266400
      L = 103                                                           00266500
      GO TO 260                                                         00266600
 592  X = .5 * ALOG( ( 1. + X ) / ( 1. - X ) )                          00266700
      GO TO 275                                                         00266800
C     ACOTH                                                             00266900
 600  IF( ABS( X ) .LT. 1. ) GO TO 602                                  00267000
      X = .5 * ALOG( ( X + 1. ) / ( X - 1. ) )                          00267100
      GO TO 275                                                         00267200
 602  L = 103                                                           00267300
      GO TO 260                                                         00267400
  610 X = QNORML( X )                                                   00267500
      GO TO 275                                                         00267600
C     INTEGER                                                           00267700
  620 X = AINT( X)                                                      00267800
      GO TO 275                                                         00267900
C     FRACTIONAL                                                        00268000
  630 X = X - AINT( X )                                                 00268100
      GO TO 275                                                         00268200
      END                                                               00268300
C  45  40      SUBROUTINE GENER                    2 19 68              00268400
      SUBROUTINE GENER                                                  00268500
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00268600
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00268700
      DIMENSION ARGS(100)                                               00268800
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00268900
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00269000
C     GENERATE                                                          00269100
C         NARGS MUST BE .GE. 4 AND EVEN                                 00269200
      IF( NARGS .GE. 4 .AND. MOD( NARGS, 2 ) .EQ. 0 ) GO TO 20          00269300
      CALL ERROR (10)                                                   00269400
      GO TO 200                                                         00269500
C     GET STORAGE COLUMN ADDRESS                                        00269600
  20  CALL ADRESS( NARGS, J )                                           00269700
      IF( J .GT. 0 ) GO TO 30                                           00269800
      CALL ERROR(3)                                                     00269900
      GO TO 200                                                         00270000
  30  IF( NERROR .NE. 0 ) GO TO 200                                     00270100
C     CONVERT INTEGERS TO FLOATING POINT                                00270200
      DO 40 I = 2, NARGS                                                00270300
      IF( KIND( I-1 ) .EQ. 0 ) ARGS( I-1 ) = IARGS( I-1 )               00270400
  40  CONTINUE                                                          00270500
      RC( J ) = ARGS( 1 )                                               00270600
      NDROW = J + NROW - 1                                              00270700
      DO 130 I = 4, NARGS, 2                                            00270800
      S = SIGN( 1., ARGS( I - 2 ) )                                     00270900
      ENDER = ARGS( I - 1 ) - .01 * ARGS( I - 2 )                       00271000
 100  J = J + 1                                                         00271100
      RC( J ) = RC( J - 1 ) + ARGS( I - 2 )                             00271200
      IF( S * ( RC( J ) - ENDER ) ) 110, 120, 120                       00271300
C         NOT DONE                                                      00271400
 110  IF( J .LT. NDROW ) GO TO 100                                      00271500
C         EXCEEDED COLUMN LENGTH                                        00271600
      CALL ERROR( 201 )                                                 00271700
      GO TO 150                                                         00271800
C         PASSES GENERATE UPPER BOUND, SET IN UPPER BOUND               00271900
 120  RC( J ) = ARGS( I - 1 )                                           00272000
 130  CONTINUE                                                          00272100
 150  NRMAX = MAX0( NRMAX, J - NDROW + NROW )                           00272200
 200  RETURN                                                            00272300
      END                                                               00272400
C  46  61      SUBROUTINE HEADS                    2 19 68              00272500
      SUBROUTINE HEADS                                                  00272600
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00272700
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00272800
      DIMENSION ARGS(100),NDIGIT(10)                                    00272900
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00273000
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00273100
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00273200
      DIMENSION LHEAD(2,8),K(6)                                         00273300
      DATA NDIGIT / 1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9 /           00273400
      DATA KOLUMN /6HCOLUMN/                                            00273500
      DATA IBLANK / 1H  /                                               00273600
C                                                                       00273700
C     THIS SUBROUTINE INSERTS THE HEADINGS OVER THE COLUMNS WHEN THE    00273800
C     STANDARD FORMAT IS CALLED  FOR.                                   00273900
C                                                                       00274000
C                                                                       00274100
C     THIS ROUTINE IS NON-STANDARD. IT HAS BEEN IMPLEMENTED HERE        00274200
C     ASSUMING B5500 WORD SIZE                                          00274300
C                                                                       00274400
C     IN IARGS(51) THRU IARGS(NARGS+50) IS A LIST OF COLUMN NUMBERS.    00274500
C     IF THE NUMBER IS .LE. 50, A 12-CHARACTER HEADING IS TO BE TAKEN   00274600
C     FROM ARRAY IHEAD AS SETUP BY SUBROUTINE XHEAD. IF .GT. 50, THE    00274700
C     HEADING   COLUMN XXXX    IS TO BE USED WHERE XXXX IS THE NUMBER,  00274800
C     CONVERTED FOR DECIMAL PRINTOUT.  THE HEADINGS ARE TO BE PRINTED   00274900
C     OVER THE DATA WHICH IS IN FORMAT  1P8E15.6                        00275000
C     IF HEADING = 0, SETUP  COLUMN XXXX                                00275100
C                                                                       00275200
      NARGS = MIN0( NARGS, 8 )                                          00275300
      DO 100 I = 1, NARGS                                               00275400
      J = IARGS( I+50 )                                                 00275500
      IF( J .GT. 50 ) GO TO 30                                          00275600
      IF( IHEAD( 1, J ) .EQ. 0 ) GO TO 30                               00275700
      LHEAD( 1, I ) = IHEAD( 1, J )                                     00275800
      LHEAD( 2, I ) = IHEAD( 2, J )                                     00275900
      GO TO 100                                                         00276000
C                                                                       00276100
C     GENERATE   COLUMN XXXX                                            00276200
C                                                                       00276300
   30 LHEAD(1,I)=KOLUMN                                                 00276400
      LL=J                                                              00276500
      DO 40 L=1,6                                                       00276600
      K( L ) = LL / (10**(6-L ))                                        00276700
      LL = LL - K( L ) * ( 10**(6-L) )                                  00276800
   40 K( L ) = K( L ) + 1                                               00276900
      KK = 0                                                            00277000
      DO 45 L =1,6                                                      00277100
      LL = K( L )                                                       00277200
      IF ( LL .EQ. 1 .AND. KK .EQ. 0 ) GO TO 44                         00277300
      KK = 1                                                            00277400
      K( L ) = NDIGIT( LL )                                             00277500
      GO TO 45                                                          00277600
   44 K( L ) = IBLANK                                                   00277700
   45 CONTINUE                                                          00277800
      CALL PK5500(6,K,LHEAD(2,I))                                       00277900
 100  CONTINUE                                                          00278000
      WRITE( IPRINT, 200 ) ( ( LHEAD(I,J), I = 1,2),J=1,NARGS)          00278100
  200 FORMAT(8(3X,2A6))                                                 00278200
      RETURN                                                            00278300
      END                                                               00278400
C  47  95      SUBROUTINE IFS                      2 19 68              00278500
      SUBROUTINE IFS                                                    00278600
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00278700
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00278800
      DIMENSION ARGS(100)                                               00278900
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00279000
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00279100
      COMMON / BLOCKX / INDEX( 6, 8 ), LEVEL                            00279200
      DIMENSION II( 3 ), K( 3 ), NNN( 6 )                               00279300
      EQUIVALENCE ( I1, II(1) ), ( I2, II(2) ), ( I3, II(3) )           00279400
C                                                                       00279500
C                 THIS COMMAND MAY APPEAR ONLY AS A STORED COMMAND.     00279600
C                                                                       00279700
C                                                                       00279800
C     IFLT, IFEQ, IFGT, IFGE, IFNE, IFLE CORRESPOND TO L2 = 9, 14       00279900
C                                                                       00280000
C     COMMANDS MAY HAVE 2 OR 3 ARGUMENTS (ONLY IFEQ AND IFNE MAY HAVE 3)00280100
C     ANY ARGUMENT MAY BE OF ANY TYPE, COLUMN NUMBER OR CONSTANT.       00280200
C                                                                       00280300
C     IN IFEQ AND IFNE THIRD ARG, IF GIVEN, IS TOLERANCE AND TEST GOES  00280400
C                                                                       00280500
C          I  ARG1-ARG2  I      I        I                              00280600
C          I  ---------  I .LT. I  ARG3  I                              00280700
C          I    ARG2     I      I        I                              00280800
C                                                                       00280900
C     IF NO TOLERANCE IS GIVEN, 0. IS ASSUMED                           00281000
C     A GIVEN TOLERANCE IS IGNORED ON IFLT, IFLE, IFGT, IFGE            00281100
C     EXAMPLES OF HOW COMMANDS READ.                                    00281200
C     IFLT  8.32 LT EVERY ENTRY OF COL 34, CONDITION IS TRUE            00281300
C     IFGE EACH ELEM COL 1 .GE. CORRESP. ELEM. COL 5, COND. IS TRUE     00281400
C     IFEQ 2. .EQ. 5. CONDITION TRUE (USEFUL WHEN INCREMENTING ARGS. )  00281500
C                                                                       00281600
C     IF CONDITION IS FALSE, NO ACTION IS TAKEN.                        00281700
C     IF CONDITION IS TRUE, THERE ARE TWO POSSIBILITIES..               00281800
C     1.  IF THE TEST COMMAND IS THE LAST ONE IN THE REPEAT LOOP        00281900
C         CURRENTLY BEING EXECUTED, THE LOOP IS TERMINATED (DROPPED     00282000
C         BACK TO THE NEXT OUTER LEVEL IF MORE THAN ONE LEVEL DEEP).    00282100
C     2.  IF THE TEST COMMAND IS NOT THE LAST ONE, ALL THAT HAPPENS IS  00282200
C         THAT THE REST OF THE LOOP IS NOT PERFORMED.  THAT IS, IF THE  00282300
C         LOOP COUNTER HAS NOT REACHED ITS UPPER  LIMIT, IT IS ADVANCED 00282400
C         ONE AND THE LOOP IS BEGUN FROM THE TOP AGAIN.                 00282500
C                                                                       00282600
      IF( LEVEL .GT. 0 ) GO TO 10                                       00282700
      CALL ERROR( 21 )                                                  00282800
      GO TO 200                                                         00282900
  10  IF( NARGS .EQ. 2 ) GO TO 30                                       00283000
      IF( NARGS .EQ. 3 ) GO TO 25                                       00283100
      CALL ERROR( 10 )                                                  00283200
      GO TO 200                                                         00283300
  20  CALL ERROR( 11 )                                                  00283400
      GO TO 200                                                         00283500
  25  IF( L2 .EQ. 10 .OR. L2 .EQ. 13 ) GO TO 30                         00283600
      CALL ERROR( 212 )                                                 00283700
      NARGS = 2                                                         00283800
  30  DO 50 I = 1, NARGS                                                00283900
      CALL ADRESS( I, II( I ) )                                         00284000
      IF( II( I ) ) 40, 20, 50                                          00284100
  40  II( I ) = -II( I )                                                00284200
  50  K( I ) = 1 - KIND( I )                                            00284300
      IF( NRMAX.NE.0 .OR. KIND(1)+KIND(2).EQ.2 ) IF( NERROR ) 200,60,20000284400
      CALL ERROR( 9 )                                                   00284500
      GO TO 200                                                         00284600
  60  NNN( 4 ) = 0                                                      00284700
      NNN( 5 ) = 0                                                      00284800
      NNN( 6 ) = 0                                                      00284900
      DO 110 I = 1, NRMAX                                               00285000
      IF( NARGS .EQ. 2 ) GO TO 65                                       00285100
C     CHECK EQ,NE WITHIN BOUNDS                                         00285200
      IF( ABS(RC(I1)/RC(I2)-1.).LT.ABS(RC(I3)) ) NNN( 5 ) = NNN( 5 ) + 100285300
      I3 = I3 + K( 3 )                                                  00285400
      GO TO 100                                                         00285500
C     CHECK IFS WITHOUT BOUNDS                                          00285600
  65  IF( RC( I1 ) - RC( I2 ) ) 70, 80, 90                              00285700
  70  NNN( 4 ) = NNN( 4 ) + 1                                           00285800
      GO TO 100                                                         00285900
  80  NNN( 5 ) = NNN( 5 ) + 1                                           00286000
      GO TO 100                                                         00286100
  90  NNN( 6 ) = NNN( 6 ) + 1                                           00286200
 100  I1 = I1 + K( 1 )                                                  00286300
 110  I2 = I2 + K( 2 )                                                  00286400
      NNN( 1 ) = NNN( 5 ) + NNN( 6 )                                    00286500
      NNN( 2 ) = NNN( 4 ) + NNN( 6 )                                    00286600
      NNN( 3 ) = NNN( 4 ) + NNN( 5 )                                    00286700
      IF( NARGS .NE. 2 ) NNN( 2 ) = NRMAX - NNN( 5 )                    00286800
      IF(NNN(L2-8).EQ.0) IF(INDEX(2,LEVEL)-INDEX(3,LEVEL))210,210,220   00286900
 200  RETURN                                                            00287000
C                                                                       00287100
C     IF-COMMAND NOT AT END OF PERFORM LOOP, ADVANCE LOOP COUNT.        00287200
C                                                                       00287300
 210  INDEX( 2, LEVEL ) = INDEX( 3, LEVEL ) + 1                         00287400
      GO TO 200                                                         00287500
C                                                                       00287600
C     IF-COMMAND IS AT END OF PERFORM LOOP, TERMINATE LOOP.             00287700
C                                                                       00287800
 220  LEVEL = LEVEL - 1                                                 00287900
      GO TO 200                                                         00288000
      END                                                               00288100
C  48  26      SUBROUTINE INPUT                    2 19 68              00288200
      SUBROUTINE INPUT                                                  00288300
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00288400
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00288500
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00288600
C                                                                       00288700
C     THIS ROUTINE HANDLES THE READING OF INPUT RECORDS.                00288800
C     IF KIO = 0, INPUT IS CARD IMAGE FROM CARD READER OR TAPE.         00288900
C     IF KIO = 1, INPUT IS REAL-TIME FROM A KEYBOARD.                   00289000
C                                                                       00289100
      KRDKNT=KRDKNT+1                                                   00289200
      IF(KIO.EQ.0)GO TO 20                                              00289300
      IF(MODE.EQ.3)GO TO 10                                             00289400
      WRITE(KBDOUT,5)                                                   00289500
   5  FORMAT(9H READY   )                                               00289600
      GO TO 20                                                          00289700
  10  WRITE(KBDOUT,15)NSTMT                                             00289800
  15  FORMAT(9H READY   ,I3,3H / )                                      00289900
      GO TO 20                                                          00290000
  20  READ(INUNIT,25)NEWCD                                              00290100
  25  FORMAT(80A1)                                                      00290200
  50  KARD(1)=0                                                         00290300
      KARD(2)=0                                                         00290400
      KARD( KRDEND+3 ) = 46                                             00290500
      CALL OMCONV( NEWCD, KARD(3), KRDEND )                             00290600
      RETURN                                                            00290700
      END                                                               00290800
C  49  79      SUBROUTINE INVCHK(A,M,N,AINV,M1,Y,  2 19 68              00290900
      SUBROUTINE INVCHK(A,M,N,AINV,M1,Y,L2,ERR,IND)                     00291000
C     INVCHK FOR OMNITAB  UNIVAC 1108   S. PEAVY  5/24/67               00291100
C     THIS SUBROUTINE INVERTS A MATRIX AND PROVIDES ALL THE CHECKS DESCR00291200
C     IN PAC-1                                                          00291300
C                                                                       00291400
C     A IS THE MATRIX TO BE INVERTED                                    00291500
C                                                                       00291600
C     M IS THE SIZE OF A AS DIMENSIONED IN THE CALLING PROGRAM  A(M,M)  00291700
C                                                                       00291800
C     N IS THE SIZE OF A TO BE INVERTED                                 00291900
C       N LESS THAN OR =M-1                                             00292000
C                                                                       00292100
C     AINV WILL CONTAIN THE INVERTED MATRIX IF INVERSION IS OBTAINABLE  00292200
C                                                                       00292300
C     M1 IS THE SIZE OF AINV AS DIMENSIONED IN THE CALLING PROGRAM      00292400
C        AINV(M1,2*M1)       M1 MUST BE GREATER OR =N+1                 00292500
C        AINV MUST HAVE TWICE AS MANY COLUMNS AS ROWS                   00292600
C        A AND AINV CANNOT BE SAME OR EQUIVALENT                        00292700
C                                                                       00292800
C     ERR  WILL CONTAIN THE 3 WAYS OF EVALUATING NORM CHECKS            00292900
C        ERR IS  A DIMENSIONED AS ERR(3)                                00293000
C                                                                       00293100
C     IND IS AN INDICATOR                                               00293200
C        IND=0  MATRIX INVERTED AND ERROR CHECKS MADE                   00293300
C        IND=1  MATRIX SINGULAR                                         00293400
C                                                                       00293500
C     COLUMN  AINV(N+1,I)   I=1,...,N  WILL CONTAIN THE ERROR BOUND OF  00293600
C     THE SUM CHECKS+1.                                                 00293700
C                                                                       00293800
      DIMENSION A(M,M),AINV(M1,M1),ERR(3),ANORM(2,3)                    00293900
      DIMENSION Y(N)                                                    00294000
      DATA ZERO/0.0/,ONE/1.0/                                           00294100
    5 NA=N                                                              00294200
   8  DO  10  I=1,NA                                                    00294300
      DO  10  J=1,NA                                                    00294400
  10  AINV(J,I)=A(J,I)                                                  00294500
      NB=NA                                                             00294600
      IF (L2.EQ.1) GO TO 11                                             00294700
      NB=NB+1                                                           00294800
      DO  3   I=1,NA                                                    00294900
      AINV(I,NA+1)=Y(I)                                                 00295000
    3 AINV(NA+1,I)=ZERO                                                 00295100
      AINV(NA+1,NA+1)=-ONE                                              00295200
      NA=NA+1                                                           00295300
  11  DO  13  I=1,NA                                                    00295400
      SUM=ZERO                                                          00295500
      AINV(NA+1,I)=ZERO                                                 00295600
      DO  12  J=1,NA                                                    00295700
  12  SUM=SUM+AINV(I,J)                                                 00295800
  13  AINV(I,NA+1)=-SUM                                                 00295900
      AINV(NA+1,NA+1)=ONE                                               00296000
      NB=NB+1                                                           00296100
  15  CALL SPINV(AINV,NB,M1,IND)                                        00296200
      IF(IND.NE.0) RETURN                                               00296300
   90 DO 170 K=1,2                                                      00296400
      DO 100 I=1,3                                                      00296500
  100 ANORM(K,I)=ZERO                                                   00296600
      DO 160 I=1,N                                                      00296700
      SUM=ZERO                                                          00296800
      DO 150 J=1,N                                                      00296900
      GO TO (110,120),K                                                 00297000
  110 TEMP= ABS(AINV(I,J))                                              00297100
      GO TO 140                                                         00297200
  120 TEMP=ZERO                                                         00297300
      DO 130 L=1,N                                                      00297400
  130 TEMP=TEMP+A(I,L)*AINV(L,J)                                        00297500
      IF(I.EQ.J) TEMP=ONE-TEMP                                          00297600
      TEMP= ABS(TEMP)                                                   00297700
  140 ANORM(K,1)=ANORM(K,1)+TEMP**2                                     00297800
      IF(ANORM(K,2).LT.TEMP) ANORM(K,2)=TEMP                            00297900
  150 SUM=SUM+TEMP                                                      00298000
      IF(ANORM(K,3).LT.SUM) ANORM(K,3)=SUM                              00298100
  160 CONTINUE                                                          00298200
      ANORM(K,1)=FSQRT(ANORM(K,1))                                      00298300
  170 ANORM(K,2)=FLOAT(N)* ANORM(K,2)                                   00298400
      DO 180 K=1,3                                                      00298500
  180 ERR(K)=(ANORM(1,K)*ANORM(2,K))/(1.  -ANORM(2,K))                  00298600
  190 RETURN                                                            00298700
      END                                                               00298800
C  50  90      SUBROUTINE INVERT                   2 19 68              00298900
      SUBROUTINE INVERT                                                 00299000
C**** MATRIX INVERSION, SOLUTION OF SYSTEM OF EQUATIONS                 00299100
C**** S PEAVY   5/22/67                                                 00299200
C**** MINVERT  (+++,+++)  SIZE +++,+++ STORE (+++,+++)                  00299300
C**** MINVERT  (+++,+++)  SIZE +++  STORE (+++,+++)                     00299400
C**** SOLVE    (+++,+++,)  SIZE +++,+++  Y VECTOR +++ STORE +++         00299500
C**** SOLVE    (+++,+++)  SIZE +++  Y VECTOR +++  STORE +++             00299600
C**** LARGEST MATRIX TO BE INVERTED OR SYSTEM TO BE SOLVED IS 50        00299700
C****                                                                   00299800
C**** L2=1   INVERT                                                     00299900
C**** L2=2   SOLVE                                                      00300000
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00300100
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00300200
      COMMON / SCRAT / A(10000),NS                                      00300300
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00300400
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00300500
     ONROW,                                                             00300600
     1NCOL,NARGS,VWXYZ(8),NERROR                                        00300700
      DIMENSION ARGS(100)                                               00300800
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00300900
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00301000
      DIMENSION IR(4),ERR(3)                                            00301100
      IF(NARGS.EQ.6.OR.NARGS.EQ.5) GO TO 1200                           00301200
      CALL ERROR(10)                                                    00301300
      RETURN                                                            00301400
 1200 J=NARGS                                                           00301500
      CALL CKIND (J)                                                    00301600
      IF(J.NE.0) GO TO 200                                              00301700
      IF (NARGS.EQ.5) GO TO 90                                          00301800
      IF (IARGS(3).NE.IARGS(4)) GO TO 210                               00301900
   90 IR(1)=IARGS(1)                                                    00302000
      IR(2)=IARGS(2)                                                    00302100
      IR(3)=IARGS(3)                                                    00302200
      IR(4)=IARGS(3)                                                    00302300
      CALL MACHK (IR,J)                                                 00302400
      IF(J.NE.0) CALL ERROR(17)                                         00302500
      IF(L2.EQ.2) GO TO 95                                              00302600
      IR(1)=IARGS(NARGS-1)                                              00302700
      IR(2)=IARGS(NARGS)                                                00302800
      CALL MACHK(IR,J)                                                  00302900
      IF(J.NE.0) CALL ERROR(17)                                         00303000
   95 IF(2*((IARGS(3)+2)**2).GT.NS ) GO TO 230                          00303100
      IF(NERROR.NE.0) RETURN                                            00303200
      CALL ADRESS(2,J)                                                  00303300
      JA=J+IARGS(1)-1                                                   00303400
      M1=IARGS(3)+1                                                     00303500
      IF(L2.EQ.2) M1=M1+1                                               00303600
      CALL ADRESS(NARGS-1,JC)                                           00303700
      CALL ADRESS(NARGS  ,JB)                                           00303800
      CALL INVCHK(RC(JA),NROW,IARGS(3),A,M1,RC(JC),L2,ERR,IND)          00303900
C**** CHECK TO SEE IF MATRIX WAS INVERTED. YES, IF IND=0                00304000
      IF(IND.NE.0) GO TO 240                                            00304100
      IA=IARGS(3)                                                       00304200
      IF(L2.EQ.2) GO TO 130                                             00304300
C**** STORE INVERTED MATRIX                                             00304400
      JB=JB+IARGS(NARGS-1)-1                                            00304500
      DO 110  I=1,IA                                                    00304600
      JC=JB                                                             00304700
      JD=(I-1)*M1+1                                                     00304800
      DO 100 J=1,IA                                                     00304900
      RC(JC)=A(JD)                                                      00305000
      JC=JC+1                                                           00305100
  100 JD=JD+1                                                           00305200
  110 JB=JB+NROW                                                        00305300
      GO TO 150                                                         00305400
C**** STORE RESULTS OF SOLUTION                                         00305500
  130 JC=M1*IARGS(3)+1                                                  00305600
      CALL ADRESS(NARGS,J)                                              00305700
      DO  140 I=1,IA                                                    00305800
      RC(J)=A(JC)                                                       00305900
      JC=JC+1                                                           00306000
  140 J=J+1                                                             00306100
C**** DETERMINE SMALLEST ERROR BOUND                                    00306200
  150 SERR = AMIN1( ERR( 1 ), ERR( 2 ), ERR( 3 ) )                      00306300
      WRITE(ISCRAT,160) SERR                                            00306400
  160 FORMAT(6X,20(1H+),43H SMALLEST ERROR BOUND ON INVERTED MATRIX IS, 00306500
     1E8.1,7H   ++++)                                                   00306600
      RETURN                                                            00306700
  200 CALL ERROR(3)                                                     00306800
      RETURN                                                            00306900
  210 CALL ERROR(210)                                                   00307000
C**** PRINT ROW AND COLUMNS DO NOT AGREE,SIZE OF COLUMNS IS SET TO ROW  00307100
      GO TO 90                                                          00307200
 230  CALL ERROR( 23 )                                                  00307300
C**** PRINT MATRIX TOO LARGE TO INVERT                                  00307400
      RETURN                                                            00307500
  240 CALL ERROR(22)                                                    00307600
C**** PRINT MATRIX IS SINGULAR OR NEAR SINGULAR-NO INVERSE              00307700
      RETURN                                                            00307800
      END                                                               00307900
C  51  18      FUNCTION LOCATE( L )                2 19 68              00308000
      FUNCTION LOCATE( L )                                              00308100
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00308200
C                                                                       00308300
C     THIS FUNCTION SEARCHES THE LIST OF STORED COMMANDS TO SEE IF ONE  00308400
C     WITH STATEMENT NUMBER L EXISTS.  IF IT DOES, RETURN ITS LOCATION. 00308500
C     IF IT DOESN"T EXIST, RETURN NEGATIVE THE LOCATION OF THE NEXT     00308600
C     HIGHER STATEMENT NUMBER.                                          00308700
C                                                                       00308800
      I = 1                                                             00308900
      AL = L                                                            00309000
  10  IF( COM( I ) - AL ) 20, 30, 40                                    00309100
   20 I = I + IFIX( COM( I+1) )                                         00309200
      GO TO 10                                                          00309300
   30 LOCATE = I                                                        00309400
      GO TO 50                                                          00309500
   40 LOCATE = -I                                                       00309600
   50 RETURN                                                            00309700
      END                                                               00309800
C  52 420      SUBROUTINE LOOKUP                   2 19 68              00309900
      SUBROUTINE LOOKUP                                                 00310000
      DIMENSION NOUT(6),NO(4),IG(40),JF(50),IR(16),N(14),IO(28),IS(28), 00310100
     1 MA(18),MM(10),MB(24),JS(12),JT(10),JZ(34),MZ(20),MJ(28),LB(10),  00310200
     2 LD(6),MX(3),NX(10)                                               00310300
      DIMENSION MV(16)                                                  00310400
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00310500
C                                                                       00310600
      DATA N(1),N(2),N(3),N(4),N(5),N(6),N(7),N(8),N(9),N(10),N(11),    00310700
     1 N(12),N(13),N(14)/10705,2604,16038,16767,17496,18225,18954,1377, 00310800
     2 15001,5*0/                                                       00310900
      DATA NOUT(1),NOUT(2),NOUT(3),NOUT(4),NOUT(5),NOUT(6) / 729, 1458, 00311000
     1 2187,2916, 3645, 4374 /                                          00311100
C                                                                       00311200
C     SUMMARY,LIST                                                      00311300
C                                                                       00311400
      DATA NO(1),NO(2),NO(3),NO(4)/14431,9522,9010,14580/               00311500
C                                                                       00311600
C     SIN,COS,TAN,COT/ARCSIN,ARCCOS,ARCTAN,ARCCOT/SIND,COSD,TAND,COSD/  00311700
C     ASIND,ACOSD,ATAND,ACOTD/ASIN,ACOS,ATAN,ACOT/                      00311800
C                                                                       00311900
      DATA IG(1),IG(2),IG(3),IG(4),IG(5),IG(6),IG(7),IG(8)/14108,0,2611,00312000
     1 0,14621,0,2612,0/                                                00312100
      DATA IG(9),IG(10),IG(11),IG(12),IG(13),IG(14),IG(15),IG(16)/1218, 00312200
     1 14108,1218,2611,1218,14621,1218,2612/                            00312300
      DATA IG(17),IG(18),IG(19),IG(20),IG(21),IG(22),IG(23),IG(24)/     00312400
     1 14108,2916,2611,2916,14621,2916,2612,2916/                       00312500
      DATA IG(25),IG(26),IG(27),IG(28),IG(29),IG(30),IG(31),IG(32)/1251,00312600
     1 10314,825,13959,1270,10314,825,14688/                            00312700
      DATA IG(33),IG(34),IG(35),IG(36),IG(37),IG(38),IG(39),IG(40)/1251,00312800
     1 10206,825,13851,1270,10206,825,14580/                            00312900
C                                                                       00313000
C     ABS,ABSOLUTE,SQRT,EXP,EXPONENT,NEGEXP,LOG,LOGE,LOGTEN,ANTILOG/    00313100
C                                                                       00313200
      DATA JF(1),JF(2),JF(3),JF(4),JF(5),JF(6),JF(7),JF(8),JF(9),JF(10),00313300
     1 JF(11),JF(12),JF(13),JF(14),JF(15),JF(16),JF(17),JF(18),JF(19),  00313400
     2 JF(20)/                                                          00313500
     3 802,0,802,11280,14328,14580,4309,0,4309,11318,10348,4309,9160,0, 00313600
     4 9160,3645,9160,14729,1127,6900/                                  00313700
C                                                                       00313800
C     SINH,COSH,TANH,COTH /ASINH,ACOSH,ATANH,ACOTH,DEVNOR,INTEGER,      00313900
C     FRACTIONAL/                                                       00314000
C                                                                       00314100
      DATA JF(21),JF(22),JF(23),JF(24),JF(25),JF(26),JF(27),JF(28)/     00314200
     1 14108,5832,2611,5832,14621,5832,2612,5832/                       00314300
      DATA JF(29),JF(30),JF(31),JF(32),JF(33),JF(34),JF(35),JF(36),     00314400
     * JF(37),JF(38),JF(39),JF(40),JF(41),JF(42) /                      00314500
     1 1251,10422,825,14067,1270,10422,825,14796,3073,10629,6959,3839,  00314600
     2 4861,2736 /                                                      00314700
C                                                                       00314800
C     ADD,SUB,MULT,DIV,RAISE,SUBTRA,MULTIPLY,DIVIDE/                    00314900
C                                                                       00315000
      DATA IR(1),IR(2),IR(3),IR(4),IR(5),IR(6),IR(7),IR(8),IR(9),IR(10),00315100
     1 IR(11),IR(12),IR(13),IR(14),IR(15),IR(16)/841,0,14420,0,10056,   00315200
     2 14580,3181,0,13158,13986,14420,15067,10056,14839,3181,6674/      00315300
C                                                                       00315400
C     GENERATE,SET,  ...  ETC.                                          00315500
C                                                                       00315600
      DATA (IO(I),I=1,28)/ 5252,4132,14006,0,4641,3753,4713,1278,12003, 00315700
     1 14580,10630,15673,14431,9522,10364,11698,14284,2322,2395,0,9793, 00315800
     2  729,9793,0,11698,3645,5590,11880/                               00315900
                                                                        00316000
                                                                        00316100
C                                                                       00316200
C     BEGIN,SCAN,REPEAT,EXECUTE,PERFORM,INCREMENT,INDEX,RESTORE         00316300
C     IFLT,IFEQ,IFGT,IFGE,IFNE,IFLE                                     00316400
C                                                                       00316500
      DATA IS(1),IS(2),IS(3),IS(4),IS(5),IS(6),IS(7),IS(8),IS(9),IS(10),00316600
     1 IS(11),IS(12),IS(13),IS(14),IS(15),IS(16),IS(17),IS(18),IS(19),  00316700
     2 IS(20),IS(21),IS(22),IS(23),IS(24),IS(25),IS(26),IS(27),IS(28) / 00316800
     3 1600,6939,13933,10206,13273,3692,4298,2774,11817,4797,6942,13270,00316900
     4 6943,4293,13276,15003,6735,14580,6728,12393,6730,14580,6730,3645,00317000
     5 6737, 3645, 6735, 3645 /                                         00317100
C                                                                       00317200
C     MDEFINE,ADEFINE,AERASE,MIDENT,ADIAG,MDIAG,MZERO,AZERO,MERASE      00317300
C                                                                       00317400
      DATA MA(1),MA(2),MA(3),MA(4),MA(5),MA(6),MA(7),MA(8),             00317500
     1     MA(9),MA(10),MA(11),MA(12),MA(13),MA(14),MA(15),MA(16),      00317600
     2 MA(17),MA(18) / 9590, 4631,                                      00317700
     3 842, 4631, 882, 1247, 9724, 4043, 846, 918,                      00317800
     4 9594, 918, 10184, 13527, 1436, 13527, 9630, 1247 /               00317900
C                                                                       00318000
C     MINVERT,LINEAR,INVERT                                             00318100
C     MMULT,MRAISE                                                      00318200
C                                                                       00318300
      DATA MM(1),MM(2),MM(3),MM(4),MM(5),MM(6),MM(7),MM(8),MM(9),MM(10)/00318400
     1 9734, 16191, 9005, 3690, 6961, 4151, 9849, 9288, 9964, 7079 /    00318500
C                                                                       00318600
C     MADD,MSUB,MTRANS,AADD,ASUB,AMULT,ADIVIDE,ARAISE,ATRANS,SCALAR,    00318700
C     ASCALAR,MSCALAR                                                   00318800
C                                                                       00318900
      DATA MB(1),MB(2),MB(3),MB(4),MB(5),MB(6),MB(7),MB(8) /            00319000
     1 9508, 2916, 10011, 1458, 10035, 1126, 760, 2916 /                00319100
      DATA MB(9),MB(10),MB(11),MB(12),MB(13),MB(14),MB(15),MB(16) /     00319200
     1 1263, 1458, 1101, 9288, 846, 16285, 1216, 7079 /                 00319300
      DATA MB(17), MB(18), MB(19), MB(20), MB(21), MB(22),MB(23),MB(24)/00319400
     1 1287, 1126,13933, 8793, 1245, 1054, 9993, 1054 /                 00319500
C                                                                       00319600
C     NLSUB,LSUB,HSUB,USUB,PSUB,TSUB                                    00319700
C                                                                       00319800
      DATA JS(1),JS(2),JS(3),JS(4),JS(5),JS(6),JS(7),JS(8),JS(9),JS(10),00319900
     1 JS(11),JS(12) / 10549, 15363, 9282, 1458, 6366, 1458, 15843,     00320000
     2 1458, 12198, 1458, 15114, 1458 /                                 00320100
C                                                                       00320200
C    PARSUM,PARPROD,RMS,AVERAGE,SUM                                     00320300
C                                                                       00320400
      DATA JT(1),JT(2),JT(3),JT(4),JT(5),JT(6),JT(7),JT(8),JT(9),JT(10)/00320500
     1 11709, 14431, 11709, 12165, 13492, 0, 1328, 13156, 14431, 0 /    00320600
C                                                                       00320700
C     ROWSUM,PRODUCT,DEFINE,      ,MAX,MAXIMUM,MIN,MINIMUM,SORT,ORDER,  00320800
C     ERASE,EXCHANGE,FLIP,CHANGE,HIERARCHY  L1=21, L2 = 1, 14           00320900
C                                                                       00321000
      DATA JZ( 1),JZ( 2),JZ( 3),JZ( 4),JZ( 5),JZ( 6),JZ( 7),JZ( 8) /    00321100
     1 13550, 14431, 12165, 3486, 3057, 6944, 9528, 0 /                 00321200
      DATA JZ( 9),JZ(10),JZ(11),JZ(12),JZ(13),JZ(14),JZ(15),JZ(16) /    00321300
     1 9528, 6933, 9734, 0, 9734, 6933, 14274, 14580 /                  00321400
      DATA JZ(17),JZ(18),JZ(19),JZ(20),JZ(21),JZ(22),JZ(23),JZ(24),     00321500
     1 JZ(25),JZ(26) / 11425, 4131, 4132, 13986, 4296, 5873, 4707,      00321600
     2 11664, 2404, 10400 /                                             00321700
      DATA JZ(27),JZ(28),JZ(29),JZ(30),JZ(31),JZ(32),JZ(33),JZ(34) /    00321800
     1 6080, 13167, 9010, 14580, 10623, 7094, 10785, 8748 /             00321900
C                                                                       00322000
C     POLYFIT,SPOLYFIT,FIT,SFIT,SOLVE,SSOLVE                            00322100
C                                                                       00322200
      DATA MZ(1),MZ(2),MZ(3),MZ(4),MZ(5),MZ(6),MZ(7),MZ(8),MZ(9),MZ(10),00322300
     1 MZ(11),MZ(12) / 12081,18396,14298,9429,4637,0,14022,14580,14268, 00322400
     2 16173, 14379, 9347 /                                             00322500
C                                                                       00322600
C     CLOSE,COUNT,SHORTEN,EXPAND,DUPLICATE,MOVE,BLOCKTRANSFER,AMOVE,    00322700
C     MMOVE,PROMOTE,DEMOTE,DIMENSION,SEPARATE,INSERT                    00322800
C                                                                       00322900
      DATA MJ(1),MJ(2),MJ(3),MJ(4),MJ(5),MJ(6),MJ(7),MJ(8),MJ(9),MJ(10),00323000
     1 MJ(11),MJ(12),MJ(13),MJ(14),MJ(15),MJ(16),MJ(17),MJ(18),MJ(19),  00323100
     2 MJ(20),MJ(21),MJ(22),MJ(23),MJ(24),MJ(25),MJ(26),MJ(27),MJ(28) / 00323200
     3 2526, 13986, 2613, 10746, 14082,                                 00323300
     4 13667,4309,1111,3499,8994,9904,3645,1797,2504,                   00323400
     5 1095,16173,9843,16173,12165,9902,3064,                           00323500
     6 11480,3172,4042,14002,1216,6958,4151 /                           00323600
C                                                                       00323700
C     STATIS,SSTATIS,FORGIVE,CHECK,FPROB                                00323800
C                                                                       00323900
      DATA LB(1),LB(2),LB(3),LB(4),LB(5),LB(6),LB(7),LB(8),LB(9),LB(10)/00324000
     1 14392,14842,14384,1278,4797,5368,2408,2484,4824,10989 /          00324100
C                                                                       00324200
C     SELECT,SEARCH,CENSOR                                              00324300
C                                                                       00324400
      DATA LD(1),LD(2),LD(3),LD(4),LD(5),LD(6) /                        00324500
     1 13998, 3746, 13987, 13211, 2336, 14274 /                         00324600
C                                                                       00324700
C     XX, X, XAX      FOR M(XX"), M(X"X), M(XAX") AND M(X"AX)           00324800
C                                                                       00324900
      DATA MX(1), MX(2), MX(3) / 18144, 17496, 17547 /                  00325000
C                                                                       00325100
C      YATES                                                            00325200
C                                                                       00325300
      DATA NX(1),NX(2) /18272,4158/                                     00325400
C                                                                       00325500
C                                                                       00325600
C     MVECDIAG,AVECDIAG,MVECMAT,AVECMAT,MMATVEC,AARRVEC,MPRINT,APRINT   00325700
C                                                                       00325800
      DATA MV(1),MV(2),MV(3),MV(4),MV(5),MV(6),MV(7),MV(8),MV(9),MV(10),00325900
     1 MV(11),MV(12),MV(13),MV(14),MV(15),MV(16) / 10076,2304,1328,2304,00326000
     2 10076,2539,1328,2232,9829,15179,774,13721,9927,6959,1179,6959 /  00326100
C*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*-.-*C 00326200
C                                                                       00326300
C     CHECK NAMES WITH QUALIFIERS FIRST                                 00326400
C                                                                       00326500
C                                                                       00326600
C     RESET     (NRMAX,COLTOP,V,W,X,Y,Z)  L1=1, L2=1,7                  00326700
C                                                                       00326800
      IF(NAME(1).NE.13276.OR.NAME(2).NE.4185)GO TO 110                  00326900
      DO 104 K=2,7                                                      00327000
      I=K                                                               00327100
      IF(NAME(3).EQ.N(I).AND.NAME(4).EQ.N(I+7))GO TO 106                00327200
 104  CONTINUE                                                          00327300
      I=1                                                               00327400
 106  L1=1                                                              00327500
      L2=I                                                              00327600
      GO TO 900                                                         00327700
C                                                                       00327800
C     PRINT     (A,B,C,D,E,F)     A = STANDARD FORMAT  L1=2, L2=1,6     00327900
C                                                                       00328000
 110  IF(NAME(1).NE.12159.OR.NAME(2).NE.10746)GO TO 120                 00328100
      L1=2                                                              00328200
      GO TO 122                                                         00328300
C                                                                       00328400
C     PUNCH     (A,B,C,D,E,F)     L1=3, L2=1,6                          00328500
C                                                                       00328600
 120  IF(NAME(1).NE.12245.OR.NAME(2).NE.2403)GO TO 130                  00328700
      L1=3                                                              00328800
 122  DO 124 L9 = 2, 7                                                  00328900
      L2=L9                                                             00329000
      IF( NAME( 3 ) .EQ. NOUT( L2-1 ) ) GO TO 900                       00329100
 124  CONTINUE                                                          00329200
      L2 = 1                                                            00329300
      GO TO 900                                                         00329400
C                                                                       00329500
C     NO        (SUMMARY,LIST)         L1=4, L2 = 1,2                   00329600
C                                                                       00329700
 130  IF(NAME(1).NE.10611)GO TO 140                                     00329800
      L1=4                                                              00329900
      DO 134 K=2,4,2                                                    00330000
      I=K                                                               00330100
      IF(NAME(3).EQ.NO(I-1).AND.NAME(4).EQ.NO(I))GO TO 136              00330200
 134  CONTINUE                                                          00330300
      GO TO 899                                                         00330400
 136  L2=I/2                                                            00330500
      GO TO 900                                                         00330600
C                                                                       00330700
C     READ (A,B,C,D,E,F)                           L1 = 5, L2 = 1, 7    00330800
C                                                                       00330900
 140  IF(NAME(1).NE.13258.OR.NAME(2).NE.2916) GO TO 150                 00331000
      L1 = 5                                                            00331100
      GO TO 122                                                         00331200
C                                                                       00331300
C     ABRIDGE (A,B,C,D,E,F)                        L1 = 6, L2 = 1, 7    00331400
C                                                                       00331500
 150  IF(NAME(1).NE.801.OR.NAME(2).NE.6676) GO TO 160                   00331600
      L1 = 6                                                            00331700
      GO TO 122                                                         00331800
C                                                                       00331900
C     DUMMY (A,B,C,D,E,F)                          L1 = 7, L2 = 1, 7    00332000
C                                                                       00332100
 160  IF(NAME(1).NE.3496.OR.NAME(2).NE.10152) GO TO 170                 00332200
      L1 = 7                                                            00332300
      GO TO 122                                                         00332400
C                                                                       00332500
C                                        THESE NAMES ARE TREATED AS     00332600
C     M(XX"), M(X"X), M(XAX"), M(X"AX)   SPECIAL CASED BY OMNITAB.      00332700
C                                        M(X"X) IS EQUIVALENT TO M X    00332800
C                                        AND THE " IS IGNORED, ETC.     00332900
C                                                                       00333000
 170  IF( NAME( 1) .NE. 9477 ) GO TO 180                                00333100
      L1 = 8                                                            00333200
      DO 174 L9 = 1, 3                                                  00333300
      L2=L9                                                             00333400
      IF( NAME(3) .EQ. MX(L2) ) GO TO 900                               00333500
 174  CONTINUE                                                          00333600
      GO TO 899                                                         00333700
C                                                                       00333800
C     MVECDIAG,AVECDIAG,MVECMAT,AVECMAT,MMATVEC,AARRVEC,MPRINT,APRINT   00333900
C       APRINT AND MPRINT HAVE OPTIONS A,B,C,D,E,F                      00334000
C                                                                       00334100
  180 DO 184 L2 = 2, 9                                                  00334200
      IF(NAME(1).EQ.MV(2*L2-3).AND.NAME(2).EQ.MV(2*L2-2)) GO TO 186     00334300
  184 CONTINUE                                                          00334400
      GO TO 190                                                         00334500
  186 L2 = L2 / 2                                                       00334600
      L1 = 27                                                           00334700
      IF(L2 .NE. 4 ) GO TO 900                                          00334800
      L1 = 9                                                            00334900
      GO TO 122                                                         00335000
  190 CONTINUE                                                          00335100
C                                                                       00335200
C     ADD,SUB,MULT,DIV,RAISE,SUBTRA,MULTIP,DIVIDE   L1 = 11, L2 = 1, 5  00335300
C                                                                       00335400
 200  DO 204 K=2,16,2                                                   00335500
      I=K                                                               00335600
      IF(NAME(1).EQ.IR(I-1).AND.NAME(2).EQ.IR(I))GO TO 206              00335700
 204  CONTINUE                                                          00335800
      GO TO 210                                                         00335900
 206  L1 = 11                                                           00336000
      L2=I/2                                                            00336100
      IF(L2.GE.6)L2=L2-4                                                00336200
      GO TO 900                                                         00336300
C                                                                       00336400
C    (SIN,COS,TAN,COT,ARCSIN,ARCCOS,ARCTAN,ARCCOT) DEGREES L1=12,L2=1,1600336500
C    ((SIND,COSD,ETC..))                                                00336600
 210  L1 = 12                                                           00336700
      DO 214 K = 2, 40, 2                                               00336800
      I=K                                                               00336900
      IF(NAME(1).EQ.IG(I-1) .AND.NAME(2).EQ.IG(I))GO TO 216             00337000
 214  CONTINUE                                                          00337100
      GO TO 220                                                         00337200
 216  L2 = I / 2                                                        00337300
      IF(L2.GT.16)L2=L2-12                                              00337400
      IF(L2.LT.8.AND.NAME(3).EQ.3058.AND.NAME(4).EQ.13262)L2=L2+8       00337500
      GO TO 900                                                         00337600
C                                                                       00337700
C     CHECK THE REST OF THE FUNCTIONS   L1 = 12 , L2 = 17,  42          00337800
C                                                                       00337900
  220 DO 224 K=2,42,2                                                   00338000
      I=K                                                               00338100
      IF(NAME(1).EQ.JF(I-1) .AND.NAME(2).EQ.JF(I))GO TO 226             00338200
 224  CONTINUE                                                          00338300
      GO TO 230                                                         00338400
 226  L2 = I / 2 + 16                                                   00338500
      GO TO 900                                                         00338600
C                                                                       00338700
C     GENERATE,SET,FIXED,FLOATING,PLOT,NOSUMM, SUMMARY,NEWPAGE,SPACE    00338800
C     CGS,MKSA,MKS,PAGE                            L1 = 13, L2 = 1, 13  00338900
C                                                                       00339000
  230 DO 234 K= 2, 28, 2                                                00339100
      I=K                                                               00339200
      IF( NAME(1).EQ.IO(I-1).AND.NAME(2).EQ.IO(I) ) GO TO 236           00339300
 234  CONTINUE                                                          00339400
      GO TO 240                                                         00339500
 236  L1 = 13                                                           00339600
      L2 = I / 2                                                        00339700
      GO TO 900                                                         00339800
C                                                                       00339900
C     BEGIN,SCAN,REPEAT,EXECUTE,PERFORM,INCREMENT,INDEX,RESTORE         00340000
C     IFLT,IFEQ,IFI ,JVI,IFLE                                           00340100
C                                                                       00340200
 240  DO 244 K = 2, 28, 2                                               00340300
      I=K                                                               00340400
      IF( NAME(1) .EQ. IS( I-1 ) .AND. NAME(2) .EQ. IS(I ) ) GO TO 246  00340500
 244  CONTINUE                                                          00340600
      GO TO 250                                                         00340700
 246  L2 = I / 2                                                        00340800
      L1 = 14                                                           00340900
      GO TO 900                                                         00341000
C                                                                       00341100
C     MDEFINE,ADEFINE,AERASE,MIDENT,ADIAG,MDIAG,MZERO,AZERO,MERASE      00341200
C     L1 = 15, L2 = 1, 4                                                00341300
C                                                           L2=1, 4     00341400
 250  DO 254 L9 = 1, 9                                                  00341500
      L2=L9                                                             00341600
      IF(NAME(1) .EQ. MA(2*L2-1) .AND. NAME(2) .EQ. MA(2*L2)) GO TO 256 00341700
 254  CONTINUE                                                          00341800
      GO TO 260                                                         00341900
 256  L1 = 15                                                           00342000
      IF( L2 .GT. 1 ) L2 = L2 - 1                                       00342100
      IF( L2 - 5 ) 900, 257, 258                                        00342200
 257  L2 = 4                                                            00342300
      GO TO 900                                                         00342400
 258  L2 = 2                                                            00342500
      GO TO 900                                                         00342600
C                                                                       00342700
C     MINVERT,LINEAR,INVERT        L1 = 16, L2 = 1, 2                   00342800
C     MMULT,MRAISE                     L1 = 17, L2 = 1, 2               00342900
C                                                                       00343000
 260  DO 264 L9 = 1, 5                                                  00343100
      L2=L9                                                             00343200
      IF(NAME(1) .EQ. MM(2*L2-1) .AND. NAME(2) .EQ. MM(2*L2)) GO TO 266 00343300
 264  CONTINUE                                                          00343400
      GO TO 270                                                         00343500
 266  L1 = 16                                                           00343600
      IF( L2 - 3 ) 900, 267, 268                                        00343700
 267  L2 = 1                                                            00343800
      GO TO 900                                                         00343900
 268  L1 = 17                                                           00344000
      L2 = L2 - 3                                                       00344100
      GO TO 900                                                         00344200
C                                                                       00344300
C     MADD,MSUB,MTRANS,AADD,ASUB,AMULT,ADIVIDE,ARAISE,ATRANS,SCALAR,    00344400
C     ASCALAR,MSCALAR                   L1 = 18, L2 = 1, 8              00344500
C                                                                       00344600
 270  DO 274 L9 = 1, 12                                                 00344700
      L2=L9                                                             00344800
      IF(NAME(1) .EQ. MB(2*L2-1) .AND. NAME(2) .EQ. MB(2*L2)) GO TO 276 00344900
 274  CONTINUE                                                          00345000
      GO TO 280                                                         00345100
 276  L1 = 18                                                           00345200
      IF( L2 - 9 ) 900, 277, 278                                        00345300
 277  L2 = 3                                                            00345400
      GO TO 900                                                         00345500
 278  L2 = 6                                                            00345600
      GO TO 900                                                         00345700
C                                                                       00345800
C     NLSUB,LSUB,HSUB,USUB,PSUB,TSUB       L1 = 19, L2 = 1, 6           00345900
C                                                                       00346000
 280  L1 = 19                                                           00346100
      DO 284 L9 = 1, 6                                                  00346200
      L2=L9                                                             00346300
      IF(NAME(1) .EQ. JS(2*L2-1) .AND. NAME(2) .EQ. JS(2*L2)) GO TO 900 00346400
 284  CONTINUE                                                          00346500
C    PARSUM,PARPROD,RMS,AVERAGE,SUM                                     00346600
C                L1 = 20,  L2 = 1, 5                                    00346700
      L1 = 20                                                           00346800
      DO 294 L9 = 1, 5                                                  00346900
      L2=L9                                                             00347000
      IF(NAME(1) .EQ. JT(2*L2-1) .AND. NAME(2) .EQ. JT(2*L2)) GO TO 900 00347100
 294  CONTINUE                                                          00347200
      L1 = 21                                                           00347300
C                                                                       00347400
C     ROWSUM,PRODUCT,DEFINE,MAX,MAXIMUM,MIN,MINIMUM,SORT,ORDER,         00347500
C      ERASE,EXCHANGE,FLIP,CHANGE,HEIRARCHY,LIST,NOLIST,NULL            00347600
C       L1 = 21,        L2 = 1, 17                                      00347700
C                                                                       00347800
      DO 304 L2=1,17                                                    00347900
      IF(NAME(1)  .EQ. JZ(2*L2-1) .AND. NAME(2) .EQ. JZ(2*L2)) GO TO 90000348000
 304  CONTINUE                                                          00348100
C                                                                       00348200
C     POLYFIT,SPOLYFIT,FIT,SFIT,SOLVE,SSOLVE                            00348300
C                        L1 = 22   L2 = 1, 6                            00348400
      L1 = 22                                                           00348500
      DO 314 L9 = 1, 6                                                  00348600
      L2=L9                                                             00348700
      IF( NAME(1) .EQ. MZ(2*L2-1) .AND. NAME(2) .EQ. MZ(2*L2)) GO TO 90000348800
 314  CONTINUE                                                          00348900
C                                                                       00349000
C     CLOSE,COUNT,SHORTEN,EXPAND,DUPLICATE,MOVE,BLOCKTRANSFER,AMOVE,    00349100
C     MMOVE,PROMOTE,DEMOTE,DIMENSION,SEPARATE,INSERT                    00349200
C                  L1 = 23, L2 = 1, 14                                  00349300
C                                                                       00349400
      L1 = 23                                                           00349500
      DO 324 L9 = 1, 14                                                 00349600
      L2=L9                                                             00349700
      IF(NAME(1).EQ.MJ(2*L2-1).AND.NAME(2).EQ.MJ(2*L2)) GO TO 900       00349800
 324  CONTINUE                                                          00349900
C                                                                       00350000
C     STATIS,SSTATIS,FORGIVE,CHECK,FPROB                                00350100
C                                                                       00350200
      L1 = 24                                                           00350300
      DO 334 L9 = 1, 5                                                  00350400
      L2=L9                                                             00350500
      IF( NAME(1).EQ.LB(2*L2-1) .AND. NAME(2).EQ.LB(2*L2) ) GO TO 900   00350600
 334  CONTINUE                                                          00350700
C                                                                       00350800
C     SELECT,SEARCH,CENSOR                                              00350900
C                                                                       00351000
      L1 = 25                                                           00351100
      DO 344 L9 = 1, 3                                                  00351200
      L2=L9                                                             00351300
      IF( NAME(1).EQ.LD(2*L2-1) .AND. NAME(2).EQ.LD(2*L2) ) GO TO 900   00351400
 344  CONTINUE                                                          00351500
      L1 = 26                                                           00351600
      IF ( NAME(1) .EQ. NX(1) .AND. NAME(2) .EQ. NX(2) ) GO TO 900      00351700
 899  L1=0                                                              00351800
      CALL NEWJOB                                                       00351900
 900  RETURN                                                            00352000
      END                                                               00352100
C  53  24      SUBROUTINE MACHK(IR,J)              2 19 68              00352200
      SUBROUTINE MACHK(IR,J)                                            00352300
C**** WORK SHEET CHECK FOR MATRIX OR ARRAY                              00352400
C**** S PEAVY   5/22/67                                                 00352500
      DIMENSION IR(4)                                                   00352600
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00352700
     ONROW,                                                             00352800
     1NCOL,NARGS,VWXYZ(8),NERROR                                        00352900
      DIMENSION ARGS(100)                                               00353000
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00353100
C**** IF J=0 EVERYTHING IS FINE                                         00353200
C**** IF J=1 NOT ENOUGH ROWS                                            00353300
C**** IF J=2 NOT ENOUGH COLUMNS                                         00353400
C**** IF J=3 NOT ENOUGH ROWS AND COLUMNS                                00353500
C**** IR(1)  STARTING ROW                                               00353600
C**** IR(2)  STARTING COLUMN                                            00353700
C**** IR(3)  NO. OF ROWS                                                00353800
C**** IR(4)  NO. OF COLUMNS                                             00353900
      JA=0                                                              00354000
      JB=0                                                              00354100
      IF( IR(1) + IR(3) - 1 .GT. NROW ) JA = 1                          00354200
      IF( IR(2) + IR(4) - 1 .GT. NCOL ) JB = 2                          00354300
      J=JA+JB                                                           00354400
      RETURN                                                            00354500
      END                                                               00354600
C  54 232      SUBROUTINE MATRIX                   2 19 68              00354700
      SUBROUTINE MATRIX                                                 00354800
C  SUBROUTINE MATRIX  R VARNER 8/24/67                                  00354900
C ****                                                                  00355000
C L2=1  ADD MATRICES A+B  MADD A(,) N,M, TO B(,) N,M AND S ORE IN C(,)  00355100
C                          MADD A(,) N,M TO B(,)     AND STORE IN C(,)  00355200
C  L2=2  SUB MATRICES A-B  MSUB A(,) N,M FROM B(,)N,M AND STORE IN C(,) 00355300
C                          MSUB A(,) N,M FROM B(,)    AND STORE IN C(,) 00355400
C  L2=3  TRANSPOSE MATRIX MTRANS A(,) N,M AND STORE IN C(,)             00355500
C        TRANSPOSE ARRAY ATRANS A(,) N,M AND STORE IN C(,)              00355600
C  L2=4  ARRAY ADD       AADD                                           00355700
C  L2=5  ARRAY SUBTRACT  ASUB                                           00355800
C  L2=6  ARRAY MULTIPLY  AMULT                                          00355900
C  L2=7  ARRAY DIVIDE    ADIV                                           00356000
C  L2=8  ARRAY RAISE     ARAISE                                         00356100
C                        GENERAL FORMS FOR ARRAY OPERATIONS             00356200
C                        A(,) N,M B(,) N,K STORE IN C(,) ARRAY BY ARRAY 00356300
C                        A(,) N,M B(,)     STORE IN C(,) ARRAY BY ARRAY 00356400
C                        A(,) N,M  K   STORE IN C(,) ARRAY BY COLUMN    00356500
C                        A(,) N    K   STORE IN C(,) ARRAY BY COLUMN    00356600
C                        A(,) N,M  X   STORE IN C(,) ARRAY BY CONSTANT  00356700
C                        A(,) N    X   STORE IN C(,) ARRAY BY CONSTANT  00356800
C ****                                                                  00356900
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00357000
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00357100
      COMMON / SCRAT / A(10000),NS                                      00357200
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00357300
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00357400
      DIMENSION ARGS(100)                                               00357500
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00357600
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00357700
      DIMENSION IR(4),ISAVE(2)                                          00357800
      EQUIVALENCE(IR,ISAVE)                                             00357900
C ****                                                                  00358000
C   CHECK TO SEE IF WE HAVE CORRECT NUMBER OF ARGUMENTS                 00358100
C   IF NOT NO FURTHER CHECKING IS DONE                                  00358200
C ****                                                                  00358300
      NPA=NARGS                                                         00358400
      IF(L2-3)100,120,140                                               00358500
 100  IF(NARGS.NE.8.AND.NARGS.NE.10) GO TO 400                          00358600
      GO TO 600                                                         00358700
  120 IF(NARGS.NE.6.AND.NARGS.NE.5) GO TO 400                           00358800
      GO TO 600                                                         00358900
 140  IF(NARGS.LT.6.OR.NARGS.GT.10.OR.NARGS.EQ.9) GO TO 400             00359000
      GO TO 600                                                         00359100
 400   CALL ERROR(10)                                                   00359200
C ****                                                                  00359300
C   CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS                          00359400
C   IF NOT NO FURTHER CHECKING IS DONE                                  00359500
C ****                                                                  00359600
  600 IF(L2.GT.3) GO TO 640                                             00359700
  605 J=NARGS                                                           00359800
      NP=NARGS                                                          00359900
      CALL CKIND(J)                                                     00360000
  610 IF(J.EQ.0) GO TO 800                                              00360100
 620  CALL ERROR(3)                                                     00360200
  640 IF(NARGS.GT.7) GO TO 605                                          00360300
      ISAVE(1)=IARGS(NARGS)                                             00360400
      IARGS(NARGS)=IARGS(NARGS-2)                                       00360500
      IARGS(NARGS-2)=IARGS(NARGS-1)                                     00360600
      IARGS(NARGS-1)=ISAVE(1)                                           00360700
      ISAVE(1)=KIND(NARGS)                                              00360800
      KIND(NARGS)=KIND(NARGS-2)                                         00360900
      KIND(NARGS-2)=KIND(NARGS-1)                                       00361000
      KIND(NARGS-1)=ISAVE(1)                                            00361100
      NARGS=NARGS-1                                                     00361200
      GO TO 605                                                         00361300
C*****                                                                  00361400
C   CHECK TO SEE IF DIMENSIONS ARE CORRECT IF THEY ARE GIVEN            00361500
C   IF NOT NO FURTHER CHECKING IS DONE                                  00361600
C ****                                                                  00361700
  800 IF(NARGS.NE.10) GO TO 1100                                        00361800
      IF(IARGS(3).EQ.IARGS(7).AND.IARGS(4).EQ.IARGS(8))GO TO 1100       00361900
      CALL ERROR(3)                                                     00362000
C ****                                                                  00362100
C   CHECK TO SEE IF ARGUMENTS ARE OUT OF RANGE                          00362200
C   IF YES NO FURTHER CHECKING IS DONE                                  00362300
C ****                                                                  00362400
 1100 IF(NARGS.LT.8) GO TO 1140                                         00362500
 1120 IP=3                                                              00362600
      GO TO 1160                                                        00362700
 1140 IP=2                                                              00362800
      IF(L2.EQ.3.AND.NPA.EQ.6) GO TO 1160                               00362900
      IF(L2.GT.3.AND.NPA.EQ.7) GO TO 1160                               00363000
      IR(4)=IARGS(3)                                                    00363100
      GO TO 1165                                                        00363200
 1160 IR(4)=IARGS(4)                                                    00363300
 1165 IR(3)=IARGS(3)                                                    00363400
      IROW = IR(3)                                                      00363500
      ICOL = IR(4)                                                      00363600
      JP=1                                                              00363700
      DO  1240   I=1,IP                                                 00363800
      IF(I.EQ.3) GO TO 1180                                             00363900
      IR(1)=IARGS(JP)                                                   00364000
      IR(2)=IARGS(JP+1)                                                 00364100
      IF(L2.EQ.3.AND.NPA.EQ.6.AND.I.EQ.2) GO TO 1170                    00364200
      GO TO 1200                                                        00364300
 1170 IR(3)=IARGS(4)                                                    00364400
      IR(4)=IARGS(3)                                                    00364500
 1180 IR(1)=IARGS(5)                                                    00364600
      IR(2)=IARGS(6)                                                    00364700
 1200 CALL MACHK(IR,J)                                                  00364800
      JP=NARGS-1                                                        00364900
      IF(J.EQ.0) GO TO 1240                                             00365000
      CALL ERROR(17)                                                    00365100
 1240 CONTINUE                                                          00365200
C ***$                                                                  00365300
C   FIND ADRESSES OF COLUMNS                                            00365400
C *****                                                                 00365500
      IF(L2-3)1500,1600,1640                                            00365600
 1500 ISAVE(1)=IARGS(1)                                                 00365700
      ISAVE(2)=IARGS(3)                                                 00365800
      IARGS(1)=IARGS(2)                                                 00365900
      IARGS(2)=IARGS(6)                                                 00366000
      IARGS(3)=IARGS(NARGS)                                             00366100
      NARGS=3                                                           00366200
      CALL CHKCOL(J)                                                    00366300
      IARGS(1)=IARGS(1)+ISAVE(1)-1                                      00366400
      IARGS(2)=IARGS(2)+IARGS(5)-1                                      00366500
      IARGS( 3)=IARGS(3)+IARGS(NP-1)-1                                  00366600
      GO TO 1800                                                        00366700
 1600 ISAVE(2)=IARGS(3)                                                 00366800
 1620 ISAVE(1)=IARGS(1)                                                 00366900
      IARGS(1)=IARGS(2)                                                 00367000
      IARGS(2)=IARGS(NP)                                                00367100
      NARGS=2                                                           00367200
      CALL CHKCOL(J)                                                    00367300
      IARGS(1)=IARGS(1)+ISAVE(1)-1                                      00367400
      IARGS(2)=IARGS(2)+IARGS(NP-1)-1                                   00367500
      GO TO 1800                                                        00367600
 1640 IF (NPA.GE.8) GO TO 1500                                          00367700
      IF(KIND(NPA  ).NE.0) GO TO 1600                                   00367800
      CALL ADRESS(NPA,J)                                                00367900
      ISAVE(2)=IARGS(3)                                                 00368000
      IARGS(3)=J                                                        00368100
      GO TO 1620                                                        00368200
C *****                                                                 00368300
C   CHECK TO SEE IF THERE WERE PREVIOUS ERRORS                          00368400
C *****                                                                 00368500
 1800 IF(NERROR.NE.0) RETURN                                            00368600
C *****                                                                 00368700
C   SUM ELEMENTS IN SCRATCH AREA                                        00368800
C   SUBTRACT ELEMENTS IN SCRATCH AREA                                   00368900
C   PRODUCTS AND QUOTIENTS FORMED USING DOUBLE PRECISION IN SCRATCH AREA00369000
C   TRANSPOSE IN SCRATCH AREA                                           00369100
C *****                                                                 00369200
      NROWPP=NROW                                                       00369300
      IF(L2-3)2000,1900,2040                                            00369400
 1900 IIB=ICOL                                                          00369500
      JJB=IROW                                                          00369600
      NROWPP=0                                                          00369700
      K=1                                                               00369800
      GO TO 2030                                                        00369900
 2000 NROWP=NROW                                                        00370000
      IBP=IARGS(2)                                                      00370100
 2020 IIB=IROW                                                          00370200
      JJB=ICOL                                                          00370300
      K=0                                                               00370400
 2030 IS=1                                                              00370500
      IAP=IARGS(1)                                                      00370600
      GO TO 2100                                                        00370700
 2040 IF(NPA.GE.8) GO TO 2000                                           00370800
      IF(KIND(NPA  ).EQ.1) GO TO 2065                                   00370900
 2060 IBP=IARGS(3)                                                      00371000
 2065 IARGS(3)=IARGS(2)                                                 00371100
      NROWP=0                                                           00371200
      GO TO 2020                                                        00371300
 2100 DO  3560  J=1,JJB                                                 00371400
      IA=IAP+(J-1)*K                                                    00371500
      IB=IBP                                                            00371600
      DO  3540  I=1,IIB                                                 00371700
      GO TO (2120,2140,2200,2220,2240,2260,2280,2300),L2                00371800
 2120 A(IS)=RC(IA)+RC(IB)                                               00371900
      GO TO 3500                                                        00372000
 2140 A(IS)=RC(IA)-RC(IB)                                               00372100
      GO TO 3500                                                        00372200
 2160 A(IS)=RC(IA)*RC(IB)                                               00372300
      GO TO 3500                                                        00372400
 2180 A(IS)=RC(IA)/RC(IB)                                               00372500
      GO TO 3500                                                        00372600
 2200 A(IS)=RC(IA)                                                      00372700
      IA=IA+NROW                                                        00372800
      GO TO 3530                                                        00372900
 2220 IF(NPA.GE.8.OR.(KIND(NPA  ).EQ.0.AND.NPA.LT.8)) GO TO 2120        00373000
      A(IS)=RC(IA)+ARGS(NPA-2)                                          00373100
      GO TO 3520                                                        00373200
 2240 IF(NPA.GE.8.OR.(KIND(NPA  ).EQ.0.AND.NPA.LT.8)) GO TO 2140        00373300
      A(IS)=RC(IA)-ARGS(NPA-2)                                          00373400
      GO TO 3520                                                        00373500
 2260 IF(NPA.GE.8.OR.(KIND(NPA  ).EQ.0.AND.NPA.LT.8)) GO TO 2160        00373600
      A(IS)=RC(IA)*ARGS(NPA-2)                                          00373700
      GO TO 3500                                                        00373800
 2280 IF(NPA.GE.8.OR.(NPA.LT.8.AND.KIND(NPA  ).EQ.0)) GO TO 2180        00373900
      A(IS)=RC(IA)/ARGS(NPA-2)                                          00374000
      GO TO 3500                                                        00374100
 2300 IF(NPA.GE.8.OR.(NPA.LT.8.AND.KIND(NPA  ).EQ.0)) GO TO 2320        00374200
      A(IS)=FEXP2(RC(IA),ARGS(NPA-2))                                   00374300
      GO TO 3500                                                        00374400
 2320 A(IS)=FEXP2(RC(IA),RC(IB))                                        00374500
      GO TO 3500                                                        00374600
 3500 IB=IB+1                                                           00374700
 3520 IA=IA+1                                                           00374800
3530  IS=IS+1                                                           00374900
 3540 CONTINUE                                                          00375000
      IAP=IAP+NROWPP                                                    00375100
      IBP=IBP+NROWP                                                     00375200
 3560 CONTINUE                                                          00375300
C *****                                                                 00375400
C   MOVE SUMS TO WORKSHEET                                              00375500
C   MOVE DIFFERENCES TO WORKSHEET                                       00375600
C   MOVE ARRAY PRODUCT TO WORKSHEET                                     00375700
C   MOVE ARRAY QUOTIENT TO SORKSHEET                                    00375800
C   MOVE TRANSPOSE TO WORKSHEET                                         00375900
C   MOVE RAISED MATRIX TO WORKSHEET                                     00376000
C *****                                                                 00376100
      IF(L2.NE.3) GO TO 3820                                            00376200
 3800 IIB=ICOL                                                          00376300
      JJB=IROW                                                          00376400
      ICP=IARGS(2)                                                      00376500
      GO TO 3840                                                        00376600
 3820 ICP=IARGS(3)                                                      00376700
 3840 IS=1                                                              00376800
 3880 DO  4080  J=1,JJB                                                 00376900
      IC=ICP                                                            00377000
      DO 4060 I=1,IIB                                                   00377100
 4000 RC(IC)=A(IS)                                                      00377200
      IC=IC+1                                                           00377300
      IS=IS+1                                                           00377400
 4060 CONTINUE                                                          00377500
      ICP=ICP+NROW                                                      00377600
 4080 CONTINUE                                                          00377700
      RETURN                                                            00377800
       END                                                              00377900
C  55  83      SUBROUTINE MDAMAD                   2 19 68              00378000
      SUBROUTINE MDAMAD                                                 00378100
C     SUBROUTINE MDAMAD                    R VARNER  9/26/67            00378200
C *****                                                                 00378300
C     SUBROUTINE TO PRE OR POST MULTIPLY A MATRIX BY A DIAGONAL STORED  00378400
C     AS A COLUMN                                                       00378500
C     L2=1      M(AD)                                                   00378600
C         MATRIX A IS POSTMULTIPLIED BY THE DIAGONAL D STORED IN COL I  00378700
C             GENERAL FORM OF COMMAND                                   00378800
C                 M(AD) A(,) N,K,  D IN COL  I  STORE IN  C(,)          00378900
C     L2=2      MDA)                                                    00379000
C         MATRIX A IS PREMULTIPLIED BY THE DIAGONAL D STORED IN COL I   00379100
C             GENERAL FORM OF COMMAND                                   00379200
C                 M(DA), A(,) N,K  K IN COL  I  STORE IN  C(,)          00379300
C *****                                                                 00379400
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00379500
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00379600
      COMMON / SCRAT / A(10000),NS                                      00379700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00379800
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00379900
      DIMENSION ARGS(100)                                               00380000
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00380100
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00380200
C ****                                                                  00380300
C     CHECK FOR CORRECT NUMBER OF ARGUMENTS                             00380400
C *****                                                                 00380500
      IF(NARGS.NE.7)CALL ERROR(10)                                      00380600
C *****                                                                 00380700
C     CHECK TO SEE THAT ALL ARGUMENTS ARE INTEGERS                      00380800
C *****                                                                 00380900
      J=NARGS                                                           00381000
      CALL CKIND(J)                                                     00381100
      IF(J.NE.0)  CALL ERROR(3)                                         00381200
C *****                                                                 00381300
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE                       00381400
C     COMPUTE ADDRESSES OF COLUMNS                                      00381500
C *****                                                                 00381600
      IARGS(12)=IARGS(4)                                                00381700
      IARGS(11)=IARGS(3)                                                00381800
      IARGS(10)=IARGS(7)                                                00381900
      IARGS(9)=IARGS(6)                                                 00382000
      IARGS(8)= 1                                                       00382100
      GO TO (100, 120 ),L2                                              00382200
 100  IARGS(7)=IARGS(4)                                                 00382300
      GO TO 140                                                         00382400
 120  IARGS(7)=IARGS(3)                                                 00382500
 140  IARGS(6)=IARGS(5)                                                 00382600
      IARGS(5)= 1                                                       00382700
      J=3                                                               00382800
      CALL MTXCHK(J)                                                    00382900
      IF(J-1) 190 , 160  ,180                                           00383000
 160  CALL ERROR(3)                                                     00383100
      RETURN                                                            00383200
 180  CALL ERROR(17)                                                    00383300
      RETURN                                                            00383400
C *****                                                                 00383500
C     CHECK FOR PREVIOUS ERRORS                                         00383600
C *****                                                                 00383700
  190 IF(NERROR.NE.0) RETURN                                            00383800
      IP=IARGS(4)                                                       00383900
      JP=IARGS(3)                                                       00384000
      GO TO ( 200, 220 ) ,L2                                            00384100
 220  I1=0                                                              00384200
      I2=1                                                              00384300
      GO TO 260                                                         00384400
 200  I1=1                                                              00384500
      I2=0                                                              00384600
 260  IA=IARGS(1)                                                       00384700
      IDP=IARGS(5)                                                      00384800
      IB=IARGS(9)                                                       00384900
      DO 340  I=1,IP                                                    00385000
      ID=IDP                                                            00385100
      DO 300  J=1,JP                                                    00385200
      RC(IB)=RC(ID)*RC(IA)                                              00385300
      ID=ID+I2                                                          00385400
      IA=IA+1                                                           00385500
      IB=IB+1                                                           00385600
 300  CONTINUE                                                          00385700
      IB=IB+NROW-JP                                                     00385800
      IA=IA+NROW-JP                                                     00385900
      IDP=IDP+I1                                                        00386000
 340  CONTINUE                                                          00386100
      RETURN                                                            00386200
      END                                                               00386300
C  56 182      SUBROUTINE MISC2                    2 19 68              00386400
      SUBROUTINE MISC2                                                  00386500
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00386600
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00386700
      DIMENSION ARGS(100)                                               00386800
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00386900
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00387000
      COMMON / SCRAT / A(10000),NS                                      00387100
C     SUBROUTINE BY CARLA MESSINA  NSRDS - NBS  JULY 1967               00387200
C                                                                       00387300
C TYPE 4 IS   EXPAND $$ TO ,, POWER IN INTERVALS OF ,, START STORE ++   00387400
C               THE POWERS MAY BE INTEGER OR NOT                        00387500
C TYPE 1 IS   CLOSE UP ROWS HAVING ** IN ++,++, ETC                     00387600
C TYPE 2 IS   COUNT LENGTH OF COLUMN ++, STORE IN COLUMN ++             00387700
C TYPE 3 IS   SHORTEN COL ++ FOR COL ++ = ** STORE IN COL ++ AND COL ++ 00387800
C TYPE 5 IS   DUPLICATE ,, TIMES THE ARRAY IN ,, ++ R=,, C=,, START     00387900
C               STORING IN ,, ++                                        00388000
C                                                                       00388100
      DIMENSION  IR(4)                                                  00388200
      J=NARGS                                                           00388300
      IF (NARGS - 2) 10,40,40                                           00388400
10    K = 10                                                            00388500
20    CALL ERROR(K)                                                     00388600
30    RETURN                                                            00388700
40    GO TO  (50,75,50 ,400,600) , L2                                   00388800
50    IF (KIND(L2))   60,60,70                                          00388900
60    K = 3                                                             00389000
      GO TO 20                                                          00389100
70    KIND(L2) = 0                                                      00389200
      ARG1 = ARGS(L2)                                                   00389300
      IARGS(L2) = IARGS(L2+1)                                           00389400
75    CALL CHKCOL(J)                                                    00389500
      IF (J)    60,80,60                                                00389600
80    DO  90 I=1,NARGS                                                  00389700
90    IARGS(I)  = IARGS(I) - 1                                          00389800
      IF (L2 - 2) 110,110,100                                           00389900
100   IF (NARGS - 5)   10,110,10                                        00390000
110   IF (NERROR .NE. 0) GO TO 30                                       00390100
      IF (NRMAX)  120,120,130                                           00390200
120   K = 9                                                             00390300
      GO TO 20                                                          00390400
130   IF  (L2 - 2) 140,200,300                                          00390500
C CLOSE UP                                                              00390600
140   DO 190 J=2,NARGS                                                  00390700
      K = IARGS(J)                                                      00390800
      M = 0                                                             00390900
      DO  160 I=1,NRMAX                                                 00391000
      J1 = K + I                                                        00391100
  148 IF ( RC( J1 ) - ARG1 ) 160,150,160                                00391200
  150 M = M + 1                                                         00391300
      IF ( (M+I) .EQ. (NRMAX+1) ) GO TO 161                             00391400
      K1 = J1 +1                                                        00391500
      K3 = K + NRMAX                                                    00391600
      DO 155 K2 = K1,K3                                                 00391700
  155 RC( K2 - 1 ) = RC( K2 )                                           00391800
      GO TO 148                                                         00391900
  160 CONTINUE                                                          00392000
  161 IF ( M .EQ. 0 ) GO TO 190                                         00392100
      M = NRMAX - M + 1                                                 00392200
      DO 180 I = M,NRMAX                                                00392300
      J1 = K + I                                                        00392400
180   RC(J1) = 0.0                                                      00392500
190   CONTINUE                                                          00392600
      GO TO 30                                                          00392700
C  COUNT                                                                00392800
200   IF (NRMAX - 2)  260,260,210                                       00392900
210   DO 250 I=3,NRMAX                                                  00393000
      J = IARGS(1) + I                                                  00393100
      IF (RC(J-2))  250,220,250                                         00393200
220   IF (RC(J-1))  250,230,250                                         00393300
230   IF (RC(J  ))  250,240,250                                         00393400
240   ARG1 = I - 3                                                      00393500
      GO TO 270                                                         00393600
250   CONTINUE                                                          00393700
260   ARG1 = NRMAX                                                      00393800
270   IARGS(2) = IARGS(2) + 1                                           00393900
      CALL VECTOR (ARG1,IARGS(2))                                       00394000
      GO TO 30                                                          00394100
C  SHORTEN                                                              00394200
300   IF (NRMAX - 2)  30,310,310                                        00394300
310   DO 360 K=2,NRMAX                                                  00394400
      J1 = IARGS(2) + K                                                 00394500
      IF (ARG1 - RC(J1-1))   320,330,340                                00394600
320   IF (ARG1 - RC(J1  ))   360,350,350                                00394700
330   NRMAX = K - 1                                                     00394800
      GO TO 370                                                         00394900
340   IF (ARG1 - RC(J1))     350,350,360                                00395000
350   NRMAX = K                                                         00395100
      GO TO 370                                                         00395200
360   CONTINUE                                                          00395300
      K = 203                                                           00395400
      CALL ERROR(K)                                                     00395500
370   DO  380 I=1,NRMAX                                                 00395600
      K = IARGS(1) + I                                                  00395700
      J = IARGS(4) + I                                                  00395800
      M = IARGS(5) + I                                                  00395900
      K1 = IARGS(2) + I                                                 00396000
      RC(M) = RC(K1)                                                    00396100
380   RC(J)= RC(K)                                                      00396200
      GO TO 30                                                          00396300
C EXPAND                                                                00396400
400   IF (NARGS - 4)  10,410,10                                         00396500
410   CALL ADRESS(4,K1)                                                 00396600
      IF (K1)  60,60,420                                                00396700
420   IF  (KIND(1))   460,430,460                                       00396800
430   CALL ADRESS(1,IARGS(1))                                           00396900
      IF (IARGS(1))   60,60,440                                         00397000
440   K = IARGS(1) - 1                                                  00397100
      DO 450  I=1,NRMAX                                                 00397200
      J = K + I                                                         00397300
450   A(I) = RC(J)                                                      00397400
      GO TO 480                                                         00397500
460   DO 470 I=1,NRMAX                                                  00397600
470   A(I) = ARGS(1)                                                    00397700
480   IF (KIND(2))   500,490,500                                        00397800
490   ARGS(2) = IARGS(2)                                                00397900
500   IF (KIND(3))   520,510,520                                        00398000
510   ARGS(3) = IARGS(3)                                                00398100
520   IF (ARGS(2)*ARGS(3))  530,530,540                                 00398200
 530  K = 20                                                            00398300
      GO TO 20                                                          00398400
540   IF (ABS(ARGS(3)) - ABS(ARGS(2)))  550,550,530                     00398500
550   IF (NERROR .NE. 0) GO TO 30                                       00398600
      IF (NRMAX)  120,120,560                                           00398700
560   CC = ARGS(3)                                                      00398800
570   DO 580  I=1,NRMAX                                                 00398900
      K = K1-1 + I                                                      00399000
580   RC(K) = FEXP2(A(I),CC)                                            00399100
      IF (ABS(CC) - ABS(ARGS(2)))  590,30,30                            00399200
590   CC = CC + ARGS(3)                                                 00399300
      IARGS(4) = IARGS(4) + 1                                           00399400
      CALL ADRESS(4,K1)                                                 00399500
      IF  (K1)   60,60,570                                              00399600
C  DUPLICATE                                                            00399700
600   IF (NARGS - 7)   10,610,10                                        00399800
610   CALL CKIND(J)                                                     00399900
      IF (J)  60,620,60                                                 00400000
620   DO  630  I=2,5                                                    00400100
630   IR(I-1) = IARGS(I)                                                00400200
      CALL MACHK(IR,J)                                                  00400300
      IF (J)  640,650,640                                               00400400
640   K = 17                                                            00400500
      GO TO 20                                                          00400600
650   IR(1) = IARGS(6)                                                  00400700
      IR(2) = IARGS(7)                                                  00400800
      IR(3) = IARGS(1)*IARGS(4)                                         00400900
      IR(4) = IARGS(5)                                                  00401000
      CALL MACHK(IR,J)                                                  00401100
      IF (J)  640,660,640                                               00401200
660   IF (IARGS(1)-1)  60,670,670                                       00401300
670   CALL ADRESS(3,IARGS(3))                                           00401400
      CALL ADRESS(7,IARGS(7))                                           00401500
      J = IARGS(6) + IARGS(1)*IARGS(4) - 1                              00401600
      IF (NRMAX - J)  680,690,690                                       00401700
680   NRMAX = J                                                         00401800
      IF (NRMAX - NROW) 690,690,640                                     00401900
690   IF (NERROR .NE. 0) GO TO 30                                       00402000
      IEND = IARGS(1)                                                   00402100
      IX = IARGS(2) - 1                                                 00402200
      IY = IARGS(3) - NROW - 1                                          00402300
      LONG = IARGS(4)                                                   00402400
      LWIDE = IARGS(5)                                                  00402500
      J = 0                                                             00402600
      DO  700  I=1,LWIDE                                                00402700
      IY = IY + NROW                                                    00402800
      DO  700  K=1,LONG                                                 00402900
      K1 = IX + IY + K                                                  00403000
      J = J + 1                                                         00403100
700   A(J) = RC(K1)                                                     00403200
      IARGS(6) = IARGS(6) - NROW - 1                                    00403300
      IY   = IARGS(7) -LONG - 1                                         00403400
      DO 710 JJ = 1, IEND                                               00403500
      J = 0                                                             00403600
      IY = IY + LONG                                                    00403700
      IX = IARGS(6)                                                     00403800
      DO 710 I=1,LWIDE                                                  00403900
      IX = IX + NROW                                                    00404000
      DO 710 K=1,LONG                                                   00404100
      K1 = IX + IY + K                                                  00404200
      J = J + 1                                                         00404300
710   RC(K1) = A(J)                                                     00404400
      GO TO 30                                                          00404500
      END                                                               00404600
C  57  64      SUBROUTINE MKRON                    2 19 68              00404700
      SUBROUTINE MKRON                                                  00404800
C     ROUTINE WRITTEN FOR OMNITAB 11/ 3/67 BY S PEAVY                   00404900
C                                                                       00405000
C     KRONECKER PRODUCT OF TWO MATRICES A(N,C)*B(M,K)=D                 00405100
C                                                                       00405200
C     FIRST FOUR ARGUMENTS DEFINE MATRIX A STARTING POS AND SIZE        00405300
C     NEXT  FOUR ARGUMENTS DEFINE MATRIX B STARTING POS AND SIZE        00405400
C     LAST TWO ARGUMENTS INDICATE WHERE RESULT IS TO BE STORED  D       00405500
C     COMMAND IS"                                                       00405600
C     MKRON A(,, ++),R=,, C=,,*B(,, ++),R=,, C=,, STORE D(,, ++)        00405700
C                                                                       00405800
C                                                                       00405900
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00406000
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00406100
      DIMENSION ARGS(100)                                               00406200
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00406300
      COMMON / SCRAT / A(10000),NS                                      00406400
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00406500
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00406600
      IF(NARGS.NE.10)CALL ERROR(10)                                     00406700
      J=NARGS                                                           00406800
      CALL CKIND(J)                                                     00406900
      IF(J.NE.0) CALL ERROR(3)                                          00407000
      IF(NERROR.NE.0)RETURN                                             00407100
      IARGS(11)=IARGS(3)*IARGS(7)                                       00407200
      IARGS(12)=IARGS(4)*IARGS(8)                                       00407300
      J=3                                                               00407400
      CALL MTXCHK(J)                                                    00407500
      IF(J.EQ.0) GO TO 150                                              00407600
      CALL ERROR(17)                                                    00407700
      RETURN                                                            00407800
  150 NRA=IARGS(3)                                                      00407900
      NCA=IARGS(4)                                                      00408000
      NRB=IARGS(7)                                                      00408100
      NCB=IARGS(8)                                                      00408200
      NDS=1                                                             00408300
      KA=IARGS(1)                                                       00408400
      DO 300 ICA=1,NCA                                                  00408500
      LA=IARGS(5)                                                       00408600
      DO 250 ICB=1,NCB                                                  00408700
      K=KA                                                              00408800
      DO 200 IRA=1,NRA                                                  00408900
      T=RC(K)                                                           00409000
      K=K+1                                                             00409100
      L=LA                                                              00409200
      DO 200 IRB=1,NRB                                                  00409300
      A(NDS)=T*RC(L)                                                    00409400
      L=L+1                                                             00409500
  200 NDS=NDS+1                                                         00409600
  250 LA=LA+NROW                                                        00409700
  300 KA=KA+NROW                                                        00409800
      NRC=IARGS(11)                                                     00409900
      NCC=IARGS(12)                                                     00410000
      NDS=1                                                             00410100
      KA=IARGS(9)                                                       00410200
      DO 410 I=1,NCC                                                    00410300
      K=KA                                                              00410400
      DO 400 J=1,NRC                                                    00410500
      RC(K)=A(NDS)                                                      00410600
      NDS=NDS+1                                                         00410700
  400 K=K+1                                                             00410800
  410 KA =KA+NROW                                                       00410900
      RETURN                                                            00411000
      END                                                               00411100
C  58 156      SUBROUTINE MMULT                    2 19 68              00411200
      SUBROUTINE MMULT                                                  00411300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00411400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00411500
      DIMENSION ARGS(100)                                               00411600
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00411700
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00411800
C *****                                                                 00411900
C     SUBROUTINE TO MULTIPLY MATRICES                                   00412000
C                 GENERAL FORMS OF MMULT                                00412100
C                   MMULT A(,) N,K, BY B(,) K,M AND STORE IN C(,)       00412200
C                   MMULT A(,) N   BY B(,) N   AND  STORE IN C(,)       00412300
C                   MMULT A(,) N   BY B(,)     AND  STORE IN C(,)       00412400
C                   MMULT A(,) N,K, BY B(,) M  AND  STORE IN C(,)       00412500
C *****                                                                 00412600
      COMMON/MULTC/NS2                                                  00412700
      COMMON / SCRAT / X,NS                                             00412800
      DIMENSION A(10000)                                                00412900
      DOUBLE PRECISION X(5000), SUM                                     00413000
      DIMENSION IR(4),ISAVE(2)                                          00413100
      EQUIVALENCE(IR,ISAVE)                                             00413200
      NS2=NS/2                                                          00413300
C *****                                                                 00413400
C     CHECK TO SEE IF WE HAVE CORRECT NUMBER OF ARGUMENTS               00413500
C *****                                                                 00413600
 100  IF(NARGS.GT.10.OR.NARGS.LT.7) CALL ERROR(10)                      00413700
C *****                                                                 00413800
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS                        00413900
C *****                                                                 00414000
  600 J=NARGS                                                           00414100
      CALL CKIND(J)                                                     00414200
      IF(J.EQ.0) GO TO 800                                              00414300
      CALL ERROR(3)                                                     00414400
C *****                                                                 00414500
C     CHECK TO SEE IF DIMENSIONS ARE CORRECT                            00414600
C *****                                                                 00414700
  800 IF(NARGS.EQ.10) GO TO 840                                         00414800
      IF(NARGS.EQ.8) GO TO 860                                          00414900
      GO TO 1100                                                        00415000
  840 IF(IARGS(4).NE.IARGS(7)) GO TO 880                                00415100
      GO TO 1100                                                        00415200
  860 IF(IARGS(3).EQ.IARGS(6)) GO TO 1100                               00415300
  880 CALL ERROR(3)                                                     00415400
C *****                                                                 00415500
C     CHECK TO SEE IF ARGUMENTS ARE OUT OF RANGE                        00415600
C *****                                                                 00415700
 1100 IP=3                                                              00415800
 1160 JERR=0                                                            00415900
      DO  1320     I=1,IP                                               00416000
      IF(I.NE.1) GO TO 1200                                             00416100
      IR(1)=IARGS(1)                                                    00416200
      IR(2)=IARGS(2)                                                    00416300
      IR(3)=IARGS(3)                                                    00416400
      IROWA=IARGS(3)                                                    00416500
      IF(NARGS.GT.8) GO TO 1180                                         00416600
      IR(4)=IARGS(3)                                                    00416700
      ICOLA=IARGS(3)                                                    00416800
      GO TO 1300                                                        00416900
 1180 IR(4)=IARGS(4)                                                    00417000
      ICOLA=IARGS(4)                                                    00417100
      GO TO 1300                                                        00417200
 1200 IF(I.EQ.3) GO TO 1240                                             00417300
      IR(1)=IARGS(NARGS-1)                                              00417400
      IR(2)=IARGS(NARGS)                                                00417500
      IR(3)=IARGS(3)                                                    00417600
      IF(NARGS.GT.8) GO TO 1220                                         00417700
      IR(4)=IARGS(3)                                                    00417800
      GO TO 1300                                                        00417900
 1220 IR(4)=IARGS(NARGS-2)                                              00418000
      GO TO 1300                                                        00418100
 1240 IF(NARGS.GT.8) GO TO 1260                                         00418200
      IR(1)=IARGS(4)                                                    00418300
      IR(2)=IARGS(5)                                                    00418400
      IR(3)=IARGS(3)                                                    00418500
      IR(4)=IARGS(3)                                                    00418600
      ICOLB=IARGS(3)                                                    00418700
      GO TO 1300                                                        00418800
 1260 IR(1)=IARGS(5)                                                    00418900
      IR(2)=IARGS(6)                                                    00419000
      IF(NARGS.EQ.10) GO TO 1280                                        00419100
      IR(3)=IARGS(4)                                                    00419200
      IR(4)=IARGS(7)                                                    00419300
      ICOLB=IARGS(7)                                                    00419400
      GO TO 1300                                                        00419500
 1280 IR(3)=IARGS(7)                                                    00419600
      IR(4)=IARGS(8)                                                    00419700
      ICOLB=IARGS(8)                                                    00419800
 1300 CALL MACHK(IR,J)                                                  00419900
      IF(J.EQ.0) GO TO 1320                                             00420000
      CALL ERROR(17)                                                    00420100
      JERR=JERR+1                                                       00420200
 1320 CONTINUE                                                          00420300
C *****                                                                 00420400
C     FIND ADDRESSES OF COLUMNS                                         00420500
C *****                                                                 00420600
      NP=NARGS                                                          00420700
      ISAVE(1)=IARGS(1)                                                 00420800
      IARGS(1)=IARGS(2)                                                 00420900
      ISAVE(2)=IARGS(3)                                                 00421000
      IARGS(3)=IARGS(NARGS)                                             00421100
      NARGS=3                                                           00421200
      IF(NP.GT.8) GO TO 1540                                            00421300
      IARGS(2)=IARGS(5)                                                 00421400
      GO TO 1580                                                        00421500
 1540 IARGS(2)=IARGS(6)                                                 00421600
 1580 CALL CHKCOL(J)                                                    00421700
      IARGS(1)=IARGS(1)+ISAVE(1)-1                                      00421800
      IARGS(3)=IARGS(3)+IARGS(NP-1)-1                                   00421900
      IF(NP.GT.8) GO TO 1620                                            00422000
      IARGS(2)=IARGS(2)+IARGS(4)-1                                      00422100
      GO TO 1800                                                        00422200
 1620 IARGS(2)=IARGS(2)+IARGS(5)-1                                      00422300
C *****                                                                 00422400
C     CHECK TO SEE IF PREVIOUS ERRORS                                   00422500
C *****                                                                 00422600
 1800 IF(NERROR.NE.0) RETURN                                            00422700
C *****                                                                 00422800
C     BEGIN MULTIPLICATION                                              00422900
C *****                                                                 00423000
      ISP=1                                                             00423100
      IBP=IARGS(2)                                                      00423200
      DO 3040  ICB=1,ICOLB                                              00423300
      IAP=IARGS(1)                                                      00423400
      DO 3020  IRA=1,IROWA                                              00423500
      IS=NS2                                                            00423600
      IA=IAP                                                            00423700
      IB=IBP                                                            00423800
      DO 3000 J=1,ICOLA                                                 00423900
      X(IS)=RC(IA)*RC(IB)                                               00424000
      IS=IS-1                                                           00424100
      IA=IA+NROW                                                        00424200
      IB=IB+1                                                           00424300
 3000 CONTINUE                                                          00424400
C *****                                                                 00424500
C     CALL ROUTINE TO SORT PRODUCTS AND SUM                             00424600
C *****                                                                 00424700
      CALL SORTSM (ICOLA,SUM)                                           00424800
      A(ISP)=SUM                                                        00424900
      ISP=ISP+1                                                         00425000
 3020 IAP=IAP+1                                                         00425100
 3040 IBP=IBP+NROW                                                      00425200
C *****                                                                 00425300
C     STORE MATRIX PRODUCT                                              00425400
C *****                                                                 00425500
      IS=1                                                              00425600
      ICP=IARGS(3)                                                      00425700
      DO 8100   J=1,ICOLB                                               00425800
      IC=ICP                                                            00425900
      DO 8080  I=1,IROWA                                                00426000
      RC(IC)=A(IS)                                                      00426100
      IS=IS+1                                                           00426200
      IC=IC+1                                                           00426300
 8080 CONTINUE                                                          00426400
 8100 ICP=ICP+NROW                                                      00426500
      RETURN                                                            00426600
      END                                                               00426700
C  59 111      SUBROUTINE MOP                      2 19 68              00426800
      SUBROUTINE MOP                                                    00426900
C**** SUBROUTINE TO DO MDEFINE,ADEFINE,MZERO,AZERO,MERASE,AERASE,MEDENT 00427000
C**** S PEAVY FOR OMNITAB UNIVAC 1108 9/ 1/67                           00427100
C**** COMMANDS ARE AS FOLLOWS                                           00427200
C****                                                                   00427300
C**** II    MDEFINE  (+++,+++) N +++ VALUE ***                          00427400
C**** I     MDEFINE  (+++,++) N ++,K ++ VALUE ***                       00427500
C**** III   SAME AS I EXCEPT COMMAND IS ADEFINE                         00427600
C**** IV    SAME AS II EXCEPT COMMAND IS ADEFINE                        00427700
C**** V     MZERO  (+++,+++)  N +++,K +++                               00427800
C**** VI    MZERO  (+++,+++)  N +++                                     00427900
C**** VII   AZERO  (+++,+++)  N +++, K +++                              00428000
C**** IX    SAME  AS  V   EXCEPT COMMAND IS MERASE                      00428100
C**** VIII  AZERO  (+++,+++)  N +++                                     00428200
C**** X     SAME AS VI  EXCEPT COMMAND IS MERASE                        00428300
C**** XI    SAME AS VII EXCEPT COMMAND IS AERASE                        00428400
C**** XII   SAME AS VIII EXCEPT COMMAND IS AERASE                       00428500
C**** XIII  MIDENT  (+++,+++) N +++, N +++                              00428600
C**** XIV   MIDENT  (+++,+++)  R=+++,C=+++,X=***                        00428700
C**** XV    MDIAG  (+++,+++) N +++ N +++  COL +++                       00428800
C**** XVI   MDIAG  (+++,+++) N +++  COL  +++                            00428900
C**** VII  MDIAG  (+++,+++) N +++ N +++  VALUE ***                      00429000
C**** VIII MDIAG  (+++,+++) N +++ VALUE ***                             00429100
C**** XIX   MDIAG  (+++,+++) N +++ N +++  ITH,JTH ELEM""(+++,+++)""     00429200
C**** XX    MDIAG  (+++,+++) N +++  ITH,JTH,ELEM ""(+++,+++)""          00429300
C**** XXI   SAME AS  XV-XX EXCEPT COMMAND IS ADIAG                      00429400
C****                                                                   00429500
C**** L2=1  MDEFINE,ADEFINE                                             00429600
C**** L2=2  MZERO,AZERO,MERASE,AERASE                                   00429700
C**** L2=3  MIDENT                                                      00429800
C**** L2=4  MDIAG,ADIAG                                                 00429900
C****                                                                   00430000
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00430100
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00430200
      COMMON / SCRAT / A(10000),NS                                      00430300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00430400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00430500
      DIMENSION ARGS(100)                                               00430600
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00430700
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00430800
      DATA ONE/1.0/,ZERO/0.0/                                           00430900
      GO TO (100,150,160,180),L2                                        00431000
  100 IF(NARGS.NE.4.AND.NARGS.NE.5) CALL ERROR (10)                     00431100
      IF(KIND(NARGS).NE.1) CALL ERROR (3)                               00431200
      IF(NARGS.EQ.4) IARGS(4)=IARGS(3)                                  00431300
      CONST=ARGS(NARGS)                                                 00431400
      CONSTA=ARGS(NARGS)                                                00431500
      J=NARGS-1                                                         00431600
  105 CALL CKIND (J)                                                    00431700
      IF(J.NE.0) CALL ERROR (3)                                         00431800
      J=1                                                               00431900
      CALL MTXCHK(J)                                                    00432000
      IF(J.NE.0) CALL ERROR (17)                                        00432100
      IF(NERROR.NE.0) RETURN                                            00432200
      JB=IARGS(1)                                                       00432300
      N=IARGS(3)                                                        00432400
      K=IARGS(4)                                                        00432500
      JA=JB                                                             00432600
      IF(L2.EQ.4) GO TO 190                                             00432700
      DO 120  KA=1,K                                                    00432800
      JC=JB                                                             00432900
      DO 110  NA=1,N                                                    00433000
      RC(JC)=CONST                                                      00433100
  110 JC=JC+1                                                           00433200
      RC(JA)=CONSTA                                                     00433300
      JA=JA+NROW+1                                                      00433400
  120 JB=JB+NROW                                                        00433500
      RETURN                                                            00433600
  150 IF(NARGS.NE.3.AND.NARGS.NE.4) CALL ERROR(10)                      00433700
      CONST=ZERO                                                        00433800
      CONSTA=ZERO                                                       00433900
      J=NARGS                                                           00434000
      IF(NARGS.EQ.4) GO TO 105                                          00434100
      IARGS(4)=IARGS(3)                                                 00434200
      J=NARGS-1                                                         00434300
      GO TO 105                                                         00434400
  160 CONST=ZERO                                                        00434500
      CONSTA=ONE                                                        00434600
      J=NARGS                                                           00434700
      IF(NARGS.NE.3) GO TO 170                                          00434800
      IARGS(4)=IARGS(3)                                                 00434900
      GO TO 105                                                         00435000
  170 IF(NARGS.EQ.4.AND.KIND(4).EQ.0) GO TO 105                         00435100
      CONSTA=ARGS(NARGS)                                                00435200
      J = J-1                                                           00435300
      IF(NARGS.EQ.5) GO TO 105                                          00435400
      IF(NARGS.NE.4)  CALL ERROR (10)                                   00435500
      IARGS(4)=IARGS(3)                                                 00435600
      GO TO 105                                                         00435700
  180 J=NARGS-1                                                         00435800
      IF(NARGS.NE.4.AND.NARGS.NE.5) CALL ERROR (10)                     00435900
      IF(NARGS.EQ.5) GO TO 105                                          00436000
      IARGS(5) = IARGS(4)                                               00436100
      IARGS(4)=IARGS(3)                                                 00436200
      GO TO 105                                                         00436300
  190 IF(KIND(NARGS).EQ.0) GO TO 210                                    00436400
      DO 200  NA=1,N                                                    00436500
      RC(JB)=ARGS(NARGS)                                                00436600
  200 JB=JB+1+NROW                                                      00436700
      RETURN                                                            00436800
  210 KIND(5)=0                                                         00436900
      CALL ADRESS(5,M)                                                  00437000
      IF(M.GT.0) GO TO 220                                              00437100
      CALL ERROR (11)                                                   00437200
      RETURN                                                            00437300
  220 DO  230 NA=1,N                                                    00437400
      RC(JB)=RC(M)                                                      00437500
      M=M+1                                                             00437600
  230 JB=JB+1+NROW                                                      00437700
      RETURN                                                            00437800
      END                                                               00437900
C  60  55      SUBROUTINE MOVE                     2 19 68              00438000
      SUBROUTINE MOVE                                                   00438100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00438200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00438300
      DIMENSION ARGS(100)                                               00438400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00438500
C     THIS ROUTINE IS ALSO CALLED BLOCKTRANSFER                         00438600
      IF( NARGS .EQ. 6 ) GO TO 70                                       00438700
      K = 10                                                            00438800
  10  CALL ERROR( K )                                                   00438900
  20  RETURN                                                            00439000
  40  K = 20                                                            00439100
      GO TO 20                                                          00439200
  50  K = 11                                                            00439300
      GO TO 20                                                          00439400
  70  IARGS( 9 ) = IARGS( 1 ) + IARGS( 3 ) - 1                          00439500
      IARGS( 13 ) = IARGS( 5 ) + IARGS( 3 ) - 1                         00439600
      IF( KIND( 1 ) + KIND( 3 ) + KIND( 4 ) + KIND( 5 ) .NE. 0 )GO TO 4000439700
      IF( IARGS( 1 ) .GT. 0 .AND. IARGS( 3 ) .GT. 0 .AND. IARGS( 5 )    00439800
     1 .GT. 0 .AND. IARGS( 9 ) .LE. NROW .AND. IARGS( 13 ) .LE. NROW )  00439900
     2 GO TO 80                                                         00440000
      K = 16                                                            00440100
      GO TO 10                                                          00440200
  80  IARGS( 10 ) = IARGS( 2 ) + IARGS( 4 ) - 1                         00440300
      KIND( 10 ) = 0                                                    00440400
      IARGS( 14 ) = IARGS( 6 ) + IARGS( 4 ) - 1                         00440500
      KIND( 14 ) = 0                                                    00440600
      DO 90 I = 2, 14, 4                                                00440700
      CALL ADRESS( I , IDUMY )                                          00440800
      IARGS( I ) = IDUMY                                                00440900
      IF( IARGS( I ) ) 40, 50, 90                                       00441000
  90  IARGS( I ) = IARGS( I ) - 1                                       00441100
C                                                                       00441200
C     IF MOVE IS UP, IR = -1, IF DOWN, IR = +1                          00441300
C     IF MOVE IS LEFT, IC = -1, IF RIGHT, IC = +1                       00441400
C     DIRECTION OF MOVE IS SUCH THAT THE TWO AREAS CAN BE OVERLAPPING   00441500
C     AND IT WILL BE DONE PROPERLY.                                     00441600
C                                                                       00441700
      IR = ISIGN( 1, IARGS( 5 ) - IARGS( 1 ) )                          00441800
      IC = ISIGN( 1, IARGS( 6 ) - IARGS( 2 ) )                          00441900
      MM = IARGS( 4*IR+5 ) + IARGS( 4*IC+6 )                            00442000
      NN = IARGS( 4*IR+9 ) + IARGS( 4*IC+10 )                           00442100
      IC = IC * NROW                                                    00442200
      MMM = IARGS( 3 )                                                  00442300
      NNN = IARGS( 4 )                                                  00442400
      DO 210 J = 1, NNN                                                 00442500
      M = MM                                                            00442600
      N = NN                                                            00442700
      DO 200 I = 1, MMM                                                 00442800
      RC( N ) = RC ( M )                                                00442900
      M = M - IR                                                        00443000
 200  N = N - IR                                                        00443100
      MM = MM - IC                                                      00443200
 210  NN = NN - IC                                                      00443300
      GO TO 20                                                          00443400
      END                                                               00443500
C  61 124      SUBROUTINE MRAISE                   2 19 68              00443600
      SUBROUTINE MRAISE                                                 00443700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00443800
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00443900
      DIMENSION ARGS(100)                                               00444000
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00444100
C *****                                                                 00444200
C     SUBROUTINE TO RAISE A MATRIX TO A POWER                           00444300
C           GENERAL FORMS OF MRAISE                                     00444400
C               MRAISE A(,) N,N TO M POWER AND STORE IN  C(,)           00444500
C               MRAISE A(,) N   TO M POWER AND STORE IN  C(,)           00444600
C               M MAY BE INTEGER OR REAL                                00444700
C *****                                                                 00444800
      COMMON/MULTC/NS2                                                  00444900
      COMMON / SCRAT / X,NS                                             00445000
      DIMENSION A(10000)                                                00445100
      DOUBLE PRECISION X(5000), SUM                                     00445200
      DIMENSION IR(4),ISAVE(2)                                          00445300
      EQUIVALENCE (IR,ISAVE)                                            00445400
      NS2=NS/2                                                          00445500
C *****                                                                 00445600
C     CHECK NUMBER OF ARGUMENTS                                         00445700
C *****                                                                 00445800
      IF(NARGS.NE.7.AND.NARGS.NE.6) CALL ERROR(10)                      00445900
C *****                                                                 00446000
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGER                         00446100
C *****                                                                 00446200
      J=NARGS                                                           00446300
      CALL CKIND(J)                                                     00446400
      IF(J.EQ.0) GO TO 800                                              00446500
      IF(KIND (NARGS-2).NE.0) GO TO 620                                 00446600
      CALL ERROR (3)                                                    00446700
      GO TO 800                                                         00446800
 620  IARGS(NARGS-2)=ARGS(NARGS-2)                                      00446900
C *****                                                                 00447000
C     CHECK TO SEE IF DIMENSIONS ARE CORRECT                            00447100
C *****                                                                 00447200
  800 IF(NARGS.EQ.6) GO TO 1100                                         00447300
      IF(IARGS(3).NE.IARGS(4)) CALL ERROR(3)                            00447400
C *****                                                                 00447500
C     CHECK TO SEE IF ARGUMENTS ARE OUT OF RANGE                        00447600
C *****                                                                 00447700
 1100 IR(3)=IARGS(3)                                                    00447800
      IR(4)=IARGS(4)                                                    00447900
      IR(1)=IARGS(1)                                                    00448000
      IR(2)=IARGS(2)                                                    00448100
      CALL MACHK(IR,J)                                                  00448200
      IF(J.NE.0) CALL ERROR(17)                                         00448300
      IR(1)=IARGS(NARGS-1)                                              00448400
      IR(2)=IARGS(NARGS)                                                00448500
      CALL MACHK(IR,J)                                                  00448600
      IF(J.NE.0) CALL ERROR(17)                                         00448700
C *****                                                                 00448800
C     CHECK TO SEE IF PREVIOUS ERRORS                                   00448900
C *****                                                                 00449000
      IF(NERROR.NE.0) RETURN                                            00449100
C *****                                                                 00449200
C     FIND ADDRESSES OF COLUMNS                                         00449300
C *****                                                                 00449400
      NP=NARGS                                                          00449500
      ISAVE(1)=IARGS(1)                                                 00449600
      IARGS(1)=IARGS(2)                                                 00449700
      IARGS(2)=IARGS(NARGS)                                             00449800
      NARGS=2                                                           00449900
      CALL CHKCOL(J)                                                    00450000
      IARGS(1)=IARGS(1)+ISAVE(1)-1                                      00450100
      IARGS(2)=IARGS(2)+IARGS(NP-1)-1                                   00450200
      ISIZE=IARGS(3)                                                    00450300
C *****                                                                 00450400
C     BEGIN MULTIPLICATION                                              00450500
C *****                                                                 00450600
C *****                                                                 00450700
C     MOVE ORIGINAL MATRIX TO SCRATCH AREA  (COLUMNWISE)                00450800
C *****                                                                 00450900
      IP=IARGS(1)                                                       00451000
      IC=1                                                              00451100
      DO    4040  J=1,ISIZE                                             00451200
      DO    4020  I=1,ISIZE                                             00451300
      A(IC)=RC(IP)                                                      00451400
      IC=IC+1                                                           00451500
      IP=IP+1                                                           00451600
 4020 CONTINUE                                                          00451700
      IP=IP+NROW-ISIZE                                                  00451800
 4040 CONTINUE                                                          00451900
      NPOW=IARGS(NP-2)-1                                                00452000
      IXP=NS-ISIZE*2                                                    00452100
      DO  5040     K=1,NPOW                                             00452200
      ISAVP=IARGS(2)                                                    00452300
      IMP=NS2                                                           00452400
      IF(K.GT.1)  GO TO 4060                                            00452500
      IRP=IARGS(1)                                                      00452600
      GO TO 4070                                                        00452700
 4060 IRP=IARGS(2)                                                      00452800
 4070 DO 5040    I=1,ISIZE                                              00452900
      ISAV =ISAVP                                                       00453000
      IC=1                                                              00453100
      IZ=IRP                                                            00453200
      IX=IXP                                                            00453300
C *****                                                                 00453400
C     SAVE ROW OF MATRIX                                                00453500
C *****                                                                 00453600
      DO  4080   J=1,ISIZE                                              00453700
      A(IX)=RC(IZ)                                                      00453800
      IX=IX-1                                                           00453900
      IZ=IZ+NROW                                                        00454000
 4080 CONTINUE                                                          00454100
      DO  5020   J=1,ISIZE                                              00454200
      IX=IXP                                                            00454300
      IM=IMP                                                            00454400
      DO 5000    JP=1,ISIZE                                             00454500
      X(IM)=A(IX)*A(IC)                                                 00454600
      IM=IM-1                                                           00454700
      IX=IX-1                                                           00454800
      IC=IC+1                                                           00454900
 5000 CONTINUE                                                          00455000
      CALL SORTSM(ISIZE,SUM)                                            00455100
      RC(ISAV )=SUM                                                     00455200
      ISAV =ISAV +NROW                                                  00455300
 5020 CONTINUE                                                          00455400
      ISAVP=ISAVP+1                                                     00455500
      IRP=IRP+1                                                         00455600
 5040 CONTINUE                                                          00455700
      RETURN                                                            00455800
      END                                                               00455900
C  62  96      SUBROUTINE MSCROW                   2 19 68              00456000
      SUBROUTINE MSCROW                                                 00456100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00456200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00456300
      DIMENSION ARGS(100)                                               00456400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00456500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00456600
C      SUBROUTINE BY CARLA MESSINA 221.04      JUNE 1967                00456700
C  TYPE 1 IS       PARSUM OF COL ++ , STORE IN COL ++                   00456800
C  TYPE 2 IS       PARPRODUCT OF COL ++, STORE IN COL ++                00456900
C  TYPE 3 IS       ROOT MEAN SQUARE      RMS OF COL ++, STORE IN COL ++ 00457000
C  TYPE 4 IS       AVERAGE OF COL ++, STORE IN COL ++      (DOWN TO NRMA00457100
C  TYPE 5 IS       SUM COL ++, STORE IN COL ++   (DOWN TO NRMAX)        00457200
C                  SUM COL ++ FROM ROW ,, TO ROW ,, STORE IN COL ++     00457300
C                  SUM COL ++  FROM ROWS NUMBERED ,, ,, ,, ,, ETC STORE 00457400
C THE THREE TYPES OF SUM ARE IDENTIFIED BY THE NO. OF NARGS =2,3 AND 4 O00457500
      ELEM = 0.0                                                        00457600
      IF (NARGS - 2) 10,40,40                                           00457700
10    K = 10                                                            00457800
20    CALL ERROR(K)                                                     00457900
30    RETURN                                                            00458000
40    CALL ADRESS( 1, J1 )                                              00458100
      IF (J1) 50,50,60                                                  00458200
50    K = 3                                                             00458300
      GO TO 20                                                          00458400
60    CALL ADRESS( NARGS, J2 )                                          00458500
      IF (J2) 50,50,70                                                  00458600
70    IF (NARGS - 3) 200,80,80                                          00458700
80    IF (L2-5) 10,85,10                                                00458800
85    NARG1 = NARGS -1                                                  00458900
      DO 100 I=2,NARG1                                                  00459000
      IF (KIND(I) .NE. 0) GO TO 120                                     00459100
      IF (IARGS(I)) 120,120,90                                          00459200
90    IF (IARGS(I)-NROW) 100,100,120                                    00459300
100   CONTINUE                                                          00459400
      IF (NERROR .NE. 0) GO TO 30                                       00459500
      IF( NARGS - 4 ) 110, 110, 170                                     00459600
C                                                                       00459700
C         SUM FROM ROW ,, TO ROW ,,                                     00459800
C                                                                       00459900
110   IF (IARGS(2) - IARGS(3)) 130,130,120                              00460000
120   K = 16                                                            00460100
      GO TO 20                                                          00460200
130   IF (NRMAX) 140,140,150                                            00460300
140   K = 9                                                             00460400
      GO TO 20                                                          00460500
150   J = J1 + IARGS( 2 )                                               00460600
      ELEM = ELEM + RC( J - 1 )                                         00460700
      IARGS(2) = IARGS(2) + 1                                           00460800
      IF (IARGS(2) - IARGS(3)) 150,150,160                              00460900
160   CALL VECTOR (ELEM,J2)                                             00461000
      GO TO 30                                                          00461100
170   IF (NRMAX) 140,140,180                                            00461200
C                                                                       00461300
C         SUM DISCRETE ROWS                                             00461400
C                                                                       00461500
180   DO 190 I = 2, NARG1                                               00461600
      J = J1 + IARGS( I )                                               00461700
190   ELEM = ELEM + RC( J - 1 )                                         00461800
      GO TO 160                                                         00461900
200   IF (NERROR .NE. 0) GO TO 30                                       00462000
      IF (NRMAX) 140,140,210                                            00462100
210   FNRMAX = NRMAX                                                    00462200
C                                                                       00462300
C         PARSUM, PARPRODUCT                                            00462400
C                                                                       00462500
      IF( L2 - 3 ) 220, 280, 300                                        00462600
220   J = L2 - 1                                                        00462700
      RC( J2 ) = RC( J1 )                                               00462800
      IF( NRMAX .EQ. 1 ) GO TO 30                                       00462900
      DO 240 I = 2, NRMAX                                               00463000
      J1 = J1 + 1                                                       00463100
      J2 = J2 + 1                                                       00463200
      IF( J .EQ. 0 ) GO TO 230                                          00463300
      RC( J2 ) = RC( J2 - 1 ) * RC( J1 )                                00463400
      GO TO 240                                                         00463500
230   RC( J2 ) = RC( J2 - 1 ) + RC( J1 )                                00463600
240   CONTINUE                                                          00463700
      GO TO 30                                                          00463800
C                                                                       00463900
C         RMS                                                           00464000
C                                                                       00464100
280   DO 290 I = 1,NRMAX                                                00464200
      J = J1 + I                                                        00464300
290   ELEM = ELEM + RC( J - 1 ) ** 2                                    00464400
      ELEM = FSQRT(ELEM/FNRMAX)                                         00464500
      GO TO 160                                                         00464600
C                                                                       00464700
C         AVERAGE, SUM ENTIRE ROW                                       00464800
C                                                                       00464900
300   DO 310 I = 1,NRMAX                                                00465000
      J = J1 + I                                                        00465100
310   ELEM = ELEM + RC( J - 1 )                                         00465200
      IF (L2 - 5) 320,160,160                                           00465300
320   ELEM = ELEM/FNRMAX                                                00465400
      GO TO 160                                                         00465500
      END                                                               00465600
C  63 116      SUBROUTINE MXTX                     2 19 68              00465700
      SUBROUTINE MXTX                                                   00465800
C *****                                                                 00465900
C     SUBROUTINE TO MULTIPLY MATRIX A BY ITS TRANSPOSE                  00466000
C                OR TRANSPOSE OF MATRIX A BY MATRIX A                   00466100
C        L2=1  MULTIPLY MATRIX BY ITS TRANSPOSE                         00466200
C           GENERAL FORM OF COMMAND                                     00466300
C                M(XXT)  A(,) N,K,  STORE IN  C(,)    N,K DEFINE X      00466400
C                M(XXT)  A(,) N    STORE IN  C(,)                       00466500
C        L2=2  MULTIPLY TRANSPOSE OF MATRIX BY ITSELF                   00466600
C           GENERAL FORM OF COMMAD                                      00466700
C                M(XTX)  A(,) N,K  STORE IN  C(,)    N,K  DEFINE X      00466800
C                M(XTX)  A(,)N    STORE IN  C(,)                        00466900
C     L2 = 3 IS M(XAX")   SET L2 = 1 AND CALL TRANSF                    00467000
C *****                                                                 00467100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00467200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00467300
      COMMON / BLOCKF / NCTOP                                           00467400
      DIMENSION ARGS(100)                                               00467500
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00467600
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00467700
      COMMON / SCRAT / X,NS                                             00467800
      DIMENSION A(10000)                                                00467900
      DOUBLE PRECISION X(5000), SUM                                     00468000
      DIMENSION  IR(4),ISAVE(2)                                         00468100
      EQUIVALENCE (IR,ISAVE)                                            00468200
      COMMON / MULTC / NS2                                              00468300
C *****                                                                 00468400
C     CHECK FOR CORRECT NUMBER OF AGRUMENTS                             00468500
C     DECIDE WHETHER COMMAND IS M(XAX") OR M(X"AX)                      00468600
C     L2 = 3 MEANS M(XAX")   L2 = 2, NARGS .GT. 6 MEANS M(X"AX)         00468700
C                                                                       00468800
      IF( L2-2 ) 100, 10, 20                                            00468900
   10 IF( NARGS .LE. 6 ) GO TO 100                                      00469000
   20 L2 = 4 - L2                                                       00469100
      CALL TRANSF                                                       00469200
      RETURN                                                            00469300
  100 IF(NARGS .NE. 5 .AND. NARGS .NE. 6 ) CALL ERROR(10)               00469400
C *****                                                                 00469500
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS                        00469600
C *****                                                                 00469700
      J=NARGS                                                           00469800
      CALL CKIND(J)                                                     00469900
      IF(J.NE.0)  CALL ERROR(3)                                         00470000
C *****                                                                 00470100
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE                       00470200
C     COMPUTE ADDRESSES                                                 00470300
C *****                                                                 00470400
      IF( NARGS.EQ. 6 ) GO TO 120                                       00470500
      IARGS(6) = IARGS(NARGS)                                           00470600
      IARGS(5) = IARGS(NARGS-1)                                         00470700
      IARGS(4) = IARGS(3)                                               00470800
  120 GO TO (140,160),L2                                                00470900
  140 IARGS(8)=IARGS(3)                                                 00471000
      IARGS(7)=IARGS(3)                                                 00471100
      GO TO 200                                                         00471200
  160 IARGS(8)=IARGS(4)                                                 00471300
      IARGS(7)=IARGS(4)                                                 00471400
  200 J=2                                                               00471500
      CALL MTXCHK(J)                                                    00471600
      IF(J-1) 260, 220, 240                                             00471700
  220 CALL ERROR(3)                                                     00471800
      RETURN                                                            00471900
  240 CALL ERROR(17)                                                    00472000
      RETURN                                                            00472100
C *****                                                                 00472200
C     CHECK FOR PREVIOUS ERRORS                                         00472300
C *****                                                                 00472400
  260 IF(NERROR .NE. 0) RETURN                                          00472500
      GO TO (300,320),L2                                                00472600
  300 IP=IARGS(3)                                                       00472700
      JP=IARGS(4)                                                       00472800
      IADD1=NROW                                                        00472900
      IADD2=1                                                           00473000
      GO TO 340                                                         00473100
  320 IP=IARGS(4)                                                       00473200
      JP=IARGS(3)                                                       00473300
      IADD1=1                                                           00473400
      IADD2=NROW                                                        00473500
 340  NS2=NS/2                                                          00473600
      IC=1                                                              00473700
      IBP=IARGS(1)                                                      00473800
      DO  440   K=1,IP                                                  00473900
      IAP=IARGS(1)                                                      00474000
      DO   420   I=1,IP                                                 00474100
      IA=IAP                                                            00474200
      IB=IBP                                                            00474300
      IS=NS2                                                            00474400
      DO   400   J=1,JP                                                 00474500
      X(IS)=RC(IA)*RC(IB)                                               00474600
      IS=IS-1                                                           00474700
      IA=IA+IADD1                                                       00474800
      IB=IB+IADD1                                                       00474900
 400  CONTINUE                                                          00475000
      IAP=IAP+IADD2                                                     00475100
      CALL SORTSM(JP,SUM)                                               00475200
      A(IC) = SUM                                                       00475300
      IC=IC+1                                                           00475400
 420  CONTINUE                                                          00475500
      IBP=IBP+IADD2                                                     00475600
 440  CONTINUE                                                          00475700
C *****                                                                 00475800
C     MOVE FROM SCRATCH AREA TO STORAGE                                 00475900
C *****                                                                 00476000
      IS=1                                                              00476100
      IC = IARGS( 5 )                                                   00476200
      DO   520    I=1,IP                                                00476300
      DO   500    J=1,IP                                                00476400
      RC(IC) = A(IS)                                                    00476500
      IS=IS+1                                                           00476600
      IC=IC+1                                                           00476700
 500  CONTINUE                                                          00476800
      IC = IC + ( NROW+NCTOP-1 ) - IP                                   00476900
 520  CONTINUE                                                          00477000
      RETURN                                                            00477100
      END                                                               00477200
C  64  40      SUBROUTINE MTXCHK(J)                2 19 68              00477300
      SUBROUTINE MTXCHK(J)                                              00477400
C     S PEAVY FOR OMNITAB   9/5/67                                      00477500
C     J AS INPUT = NO OF MATRICES TO BE CHECKED                         00477600
C       IARGS(1), IARGS(5),...,IARGS(4*(J-1)+1) STARTING ROW    OF  MAT 00477700
C       IARGS(2), IARGS(6),...,IARGS(4*(J-1)+2) STARTING COLUMN OF  MAT 00477800
C       IARGS(3), IARGS(7),...,IARGS(4*(J-1)+3) NO. OF ROWS             00477900
C       IARGS(4), IARGS(8),...,IARGS(4*(J-1)+4)  NO OF COLUMNS          00478000
C                                                                       00478100
C     UPON RETURN                                                       00478200
C     J=0  IF  ALL MATRICES ARE IN WORK SHEET                           00478300
C     AND                                                               00478400
C       IARGS(1),IARGS(5),...,IARGS(4*(J-1)+1) WILL CONTAIN STARTING    00478500
C       ADDRESS OF MATRIX                                               00478600
C     J GT  ZERO  IF  MATRIX IS NOT IN WORK SHEET                       00478700
C     J=1 SOME IARGS ARE NEGATIVE,  J=2 MATRIX TO BIG FOR WORK SHEET    00478800
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00478900
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00479000
      COMMON / SCRAT / A(10000),NS                                      00479100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00479200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00479300
      DIMENSION ARGS(100)                                               00479400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00479500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00479600
      JA=J                                                              00479700
      JB=4*J                                                            00479800
      J = 0                                                             00479900
      DO  100  I=1,JB                                                   00480000
      IF(IARGS(I).GT.0) GO TO 100                                       00480100
      J=1                                                               00480200
      RETURN                                                            00480300
  100 CONTINUE                                                          00480400
      DO  120  I=1,JB,4                                                 00480500
      IF(IARGS(I)+IARGS(I+2)-1.GT.NROW) GO TO 130                       00480600
      IF(IARGS(I+1)+IARGS(I+3)-1.GT.NCOL) GO TO 130                     00480700
      CALL ADRESS(I+1,JC)                                               00480800
 120  IARGS(I)=JC+IARGS(I)-1                                            00480900
      RETURN                                                            00481000
 130  J=2                                                               00481100
      RETURN                                                            00481200
      END                                                               00481300
C  65  75      SUBROUTINE NNAME(NAME)              2 19 68              00481400
      SUBROUTINE NNAME(NAME)                                            00481500
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00481600
      DIMENSION NAME(2),MISC(6)                                         00481700
C                                                                       00481800
C THIS SUBROUTINE ASSEMBLES A NAME UP TO THE FIRST NON-LETTER OR UP TO  00481900
C SIX LETTER, WHICHEVER IS FIRST. THE INDEX, M, IS INITIALLY POINTING AT00482000
C THE FIRST LETTER, IT IS LEFT POINTING AT THE FIRST NON-LETTER.        00482100
C                                                                       00482200
C                                                                       00482300
C                                                                       00482400
C                                                                       00482500
C                                                                       00482600
C                                                                       00482700
C                                                                       00482800
C             SPACE OUT SO THAT TABLE LIES  ALL ON ONE PAGE             00482900
C                                                                       00483000
C                                                                       00483100
C                                                                       00483200
C                                                                       00483300
C                                                                       00483400
C                                                                       00483500
C                                                                       00483600
C                                                                       00483700
C                                                                       00483800
C                                                                       00483900
C                                                                       00484000
C                                                                       00484100
C        CONVERSION TABLE FOR ALPHABETIC TO NUMERIC AS USED BY OMNITAB. 00484200
C                                                                       00484300
C                  A       729        27         1                      00484400
C                  B      1458        54         2                      00484500
C                  C      2187        81         3                      00484600
C                  D      2916       108         4                      00484700
C                  E      3645       135         5                      00484800
C                  F      4374       162         6                      00484900
C                  G      5103       189         7                      00485000
C                  H      5832       216         8                      00485100
C                  I      6561       243         9                      00485200
C                  J      7290       270        10                      00485300
C                  K      8019       297        11                      00485400
C                  L      8748       324        12                      00485500
C                  M      9477       351        13                      00485600
C                  N     10206       378        14                      00485700
C                  O     10935       405        15                      00485800
C                  P     11664       432        16                      00485900
C                  Q     12393       459        17                      00486000
C                  R     13122       486        18                      00486100
C                  S     13851       513        19                      00486200
C                  T     14580       540        20                      00486300
C                  U     15309       567        21                      00486400
C                  V     16038       594        22                      00486500
C                  W     16767       621        23                      00486600
C                  X     17496       648        24                      00486700
C                  Y     18225       675        25                      00486800
C                  Z     18954       702        26                      00486900
C                                                                       00487000
C                                                                       00487100
C     THE FIRST THREE CHARACTERS GO INTO THE FIRST WORD OF NAME         00487200
C     THE SECOND THREE CHARACTERS GO INTO THE SECOND WORD OF NAME       00487300
C                                                                       00487400
C                                                                       00487500
      DO 10 I=1,6                                                       00487600
 10   MISC(I)=0                                                         00487700
      DO 20 I=1,6                                                       00487800
      L=KARD(M)-9                                                       00487900
      IF(L.LT.1.OR.L.GE.27)GO TO 40                                     00488000
      MISC(I)=L                                                         00488100
 20   M=M+1                                                             00488200
 30   IF(KARD(M).LT.10.OR.KARD(M).GE.36)GO TO 40                        00488300
      M=M+1                                                             00488400
      GO TO 30                                                          00488500
 40   NAME(1)=MISC(3)+27*(MISC(2)+27*MISC(1))                           00488600
      NAME(2)=MISC(6)+27*(MISC(5)+27*MISC(4))                           00488700
      RETURN                                                            00488800
      END                                                               00488900
C  66  13      FUNCTION NONBLA(I)                  2 19 68              00489000
      FUNCTION NONBLA(I)                                                00489100
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00489200
C                                                                       00489300
C     SCAN KARD STARTING AT KARD(I) UNTIL A NON-BLANK CHARACTER IS      00489400
C     FOUND.  POINT M AT IT AND ALSO RETURN IT AS FUNCTION VALUE.       00489500
C                                                                       00489600
      M=I                                                               00489700
   1  IF(KARD(M).NE.44)GO TO 2                                          00489800
      M=M+1                                                             00489900
      GO TO 1                                                           00490000
   2  NONBLA=KARD(M)                                                    00490100
      RETURN                                                            00490200
      END                                                               00490300
C  67  38      SUBROUTINE OMCONV( NWCD, KRD, KRDE  2 19 68              00490400
      SUBROUTINE OMCONV( NWCD, KRD, KRDEND )                            00490500
      COMMON / ABCDEF / L(48)                                           00490600
C                                                                       00490700
C     ARRAY L CONTAINS THE ALPHABET FORMATTED  1H                       00490800
C                                                                       00490900
C     THIS ROUTINE CONVERTS INPUT CARD IMAGES TO A STANDARD CODE SO     00491000
C     THAT OMNITAB CAN DEAL WITH THE CHARACTERS AS INTEGERS.            00491100
C                                                                       00491200
C                                                                       00491300
C     THIS ROUTINE IS INCLUDED ONLY FOR COMPLETENESS.  IT SHOULD BE     00491400
C     REWRITTEN IN ASSEMBLY LANGUAGE FOR EACH COMPUTER.  ALSO, IT       00491500
C     CANNOT MEET ASA STANDARDS BECAUSE ASA DOES NOT REQUIRE THAT DATA  00491600
C     READ WITH FORMAT  A1  BE STORED THE SAME AS HOLLERITH DATA  SETUP 00491700
C     WITH  1H    ALTHOUGH THEY WILL BE THE SAME ON MOST COMPUTERS.     00491800
C                                                                       00491900
C     ALSO, ASA DOESNT RECOGNIZE THE CHARACTER  "  APOSTROPHE  WHICH    00492000
C     OMNITAB EQUATES TO THE  *  ASTERISK.                              00492100
C     THIS LAST ITEM IS NOT IMPORTANT TO THE EXECUTION OF OMNITAB, IT   00492200
C     IS ONLY A CONVENIENCE.                                            00492300
C                                                                       00492400
      DIMENSION NWCD( 1 ), KRD( 1 )                                     00492500
      DO 30 I = 1, KRDEND                                               00492600
      IJK = I                                                           00492700
      K=NWCD(I)                                                         00492800
      IF(K.NE.L(45))GO TO 10                                            00492900
      J=45                                                              00493000
      GO TO 30                                                          00493100
  10  DO 20 M=1,46                                                      00493200
      J=M                                                               00493300
      IF(K.EQ.L(J))GO TO 30                                             00493400
  20  CONTINUE                                                          00493500
      IF(K.NE.L(47))GO TO 25                                            00493600
      J=41                                                              00493700
  30  KRD(I)=J-1                                                        00493800
      RETURN                                                            00493900
   25 KRD( IJK ) = 46                                                   00494000
      RETURN                                                            00494100
      END                                                               00494200
C  68 311      SUBROUTINE OMNIT                    2 19 68              00494300
      SUBROUTINE OMNIT                                                  00494400
C     *************** THIS IS THE MAIN OMNITAB ROUTINE *****************00494500
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00494600
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00494700
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00494800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00494900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00495000
      DIMENSION ARGS(100)                                               00495100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00495200
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00495300
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00495400
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00495500
      DATA IBLANK/1H /,LETSGO/-1/                                       00495600
C                                                                       00495700
C     THIS IS THE MAIN OMNITAB PROGRAM                                  00495800
C                                                                       00495900
C                                                                       00496000
C     SUBROUTINES CALLED BY THIS PROGRAM..                              00496100
C     SETUP,INPUT,ERROR,STMT,NNAME,AARGS,ASTER,SETQ,READQ,STORE,XECUTE  00496200
C     AERR,XOMNIT,XFORMT,LOOKUP                                         00496300
C                                                                       00496400
C                                                                       00496500
C     MOD = 1  INTERPRETIVE MODE                                        00496600
C          = 2  DATA MODE (READ  SET)                                   00496700
C          = 3  STORAGE MODE (BETWEEN BEGIN AND FINISH)                 00496800
C          =4  IMPLIED STORAGE MODE (STATEMENT NUMBER GIVEN)            00496900
C                                                                       00497000
C                                                                       00497100
C     0 =  0,  1 =  1, ETC., 9 =  9, A = 10, B = 11, ETC, Z= 35, / = 36 00497200
C     . = 37, - = 38, + = 39, * = 40, ( = 41, ) = 42, , = 43            00497300
C     BLANK = 44, = = 45, $ AND OTHERS = 46                             00497400
C                                                                       00497500
C-----------------------------------------------------------------------00497600
C                                                                       00497700
C     THIS IS A CALL TO   SYSTEM TO ESTABLISH THE PRINTER PAGE TO BE    00497800
C     62 LINES LONG STARTING ON LINE 3. UNIQUE TO NBS 1108 INSTALLATION.00497900
C                                                                       00498000
C     CALL PGSIZE( 3, 62 )                                              00498100
C                                                                       00498200
C-----------------------------------------------------------------------00498300
      CALL SETUP                                                        00498400
  50  IF(MODE.EQ.3)NSTMT=NSTMT+10                                       00498500
      IF(MODE.EQ.4)MODE=1                                               00498600
      NAME(1)=0                                                         00498700
      NAME(2)=0                                                         00498800
      NAME(3)=0                                                         00498900
      NAME(4)=0                                                         00499000
      NARGS=0                                                           00499100
      J=0                                                               00499200
C                                                                       00499300
C     CHECK FOR ACCUMULATED ERRORS DURING LAST EXECUTED COMMAND         00499400
C                                                                       00499500
      CALL AERR(0)                                                      00499600
  52  CALL INPUT                                                        00499700
C                                                                       00499800
C     SCANNING BEGINS WITH THE THIRD CHARACTER. THE FIRST TWO ARE DUMMY 00499900
C     TO KEEP THE PROGRAM OUT OF TROUBLE.  SCANNING TERMINATES WITH A $ 00500000
C     A $ HAS BEEN PLANTED IN THE (KRDEND+1)-TH POSITION.               00500100
C                                                                       00500200
      M=2                                                               00500300
  55  M=M+1                                                             00500400
      K=KARD(M)                                                         00500500
      IF(K.GE.36)IF(K-46)55,58,55                                       00500600
      IF(K.GE.10)GO TO 70                                               00500700
C                                                                       00500800
C     A NUMBER IS THE FIRST ALPHANUMERIC CHARACTER ENCOUNTERED, ERROR IF00500900
C     IN MODE 3                                                         00501000
C                                                                       00501100
      CALL OUTPUT                                                       00501200
      IF(MODE.NE.3)GO TO 60                                             00501300
  57  CALL ERROR(2)                                                     00501400
      GO TO 52                                                          00501500
  58  IF( MODE .NE. 4 ) CALL OUTPUT                                     00501600
      GO TO 50                                                          00501700
  60  CALL STMT(NSTMT)                                                  00501800
      IF(KARG.NE.0)IF(MODE-2)57,98,57                                   00501900
C                                                                       00502000
C     IF AN ILLEGAL STATEMENT NUMBER WAS FOUND, KARG = 1 (KARG = 0 IF   00502100
C     LEGAL)                                                            00502200
C                                                                       00502300
      MODE=4                                                            00502400
C                                                                       00502500
C     M IS POINTING AT THE FIRST LETTER ON THE CARD, ASSEMBLE NAME.     00502600
C                                                                       00502700
  70  CALL NNAME(NAME(1))                                               00502800
C                                                                       00502900
C                                                                       00503000
C     CHECK THE FIRST NAME FOR SPECIAL NAMES...                         00503100
C     OMNITAB, FORMAT, NOTE, FOOTNOTE, HEAD,TITLE                       00503200
C                                                                       00503300
C     OMNITAB                                                           00503400
C                                                                       00503500
      IF(NAME(1).NE.11300.OR.NAME(2).NE.7102)IF(LETSGO)80,81,81         00503600
C                                                                       00503700
C     IF NOT THE FIRST OMNITAB CARD, WRITE EOF RECORD.                  00503800
C                                                                       00503900
      IF (LETSGO .NE. -1 ) WRITE( ISCRAT, 75 )                          00504000
  75  FORMAT(1HZ,83X)                                                   00504100
      LETSGO=LETSGO+1                                                   00504200
  80  CALL XOMNIT(LETSGO)                                               00504300
      IF( LETSGO .NE. -1 ) GO TO 50                                     00504400
      LETSGO=0                                                          00504500
C                                                                       00504600
C     FINISH                                                            00504700
C                                                                       00504800
  81  IF(NAME(1).NE.4631.OR.NAME(2).NE.7082) GO TO 811                  00504900
      MODE = 1                                                          00505000
      GO TO 58                                                          00505100
C                                                                       00505200
C     FORMAT                                                            00505300
C                                                                       00505400
  811 IF( MODE .NE. 4 ) CALL OUTPUT                                     00505500
      IF(NAME(1).NE.4797.OR.NAME(2).NE.9524) GO TO 82                   00505600
      CALL XFORMT                                                       00505700
  812 IF(MODE.GE.3)CALL ERROR(202)                                      00505800
      IF(MODE.NE.3)MODE=1                                               00505900
      GO TO 50                                                          00506000
C                                                                       00506100
C     NOTE                                                              00506200
C                                                                       00506300
  82  IF(NAME(1).NE.10631.OR.NAME(2).NE.3645)GO TO 83                   00506400
  822 WRITE(IPRINT,825)(NEWCD(I-2),I=M,74)                              00506500
  825 FORMAT(10X,72A1)                                                  00506600
      LNCNT=LNCNT+1                                                     00506700
      GO TO 812                                                         00506800
C                                                                       00506900
C     FOOTNOTE                                                          00507000
C                                                                       00507100
  83  IF(NAME(1).NE.4794.OR.NAME(2).NE.14973)GO TO 84                   00507200
      IF(LNCNT.GE.61)GO TO 822                                          00507300
      DO 831 LNCNX=LNCNT,61                                             00507400
  831 WRITE(IPRINT,832)                                                 00507500
  832 FORMAT(1H )                                                       00507600
      LNCNT=61                                                          00507700
      GO TO 822                                                         00507800
C                                                                       00507900
C     HEAD                                                              00508000
C                                                                       00508100
  84  IF(NAME(1).NE.5968.OR.NAME(2).NE.2916)GO TO 85                    00508200
      CALL XHEAD                                                        00508300
      GO TO 812                                                         00508400
C                                                                       00508500
C     TITLES.     TITLEX=TITLE5 , TITLEY = TITLE6                       00508600
C                                                                       00508700
   85 IF ( NAME(1) .NE. 14843 ) GO TO 87                                00508800
C     CHECK NAME  TITLE                                                 00508900
      IF ( NAME(2) .EQ. 8883 ) GO TO 852                                00509000
C     CHECK TITLEX AND TITLEY                                           00509100
      K = 5                                                             00509200
      IF ( NAME(2) .NE. 8908 ) IF ( NAME(2) - 8907 ) 87,854,87          00509300
      K = 6                                                             00509400
      GO TO 854                                                         00509500
  852 K = KARD( M )                                                     00509600
      IF ( K .GE. 1 .AND. K .LE. 6 ) GO TO 854                          00509700
      CALL ERROR(209)                                                   00509800
      K = 1                                                             00509900
  854 MM = MIN0( M+59 , 81 )                                            00510000
      DO 856 I=1,60                                                     00510100
  856 ITLE(I,K) = IBLANK                                                00510200
      I = 1                                                             00510300
      DO 858 MX=M,MM                                                    00510400
      ITLE(I,K) = NEWCD( MX-1 )                                         00510500
  858 I = I + 1                                                         00510600
      GO TO 812                                                         00510700
C                                                                       00510800
C         STOP                                                          00510900
C                                                                       00511000
  87  IF(NAME(1).NE.14406.OR.NAME(2).NE.11664)GO TO 89                  00511100
      WRITE( ISCRAT, 75 )                                               00511200
      CALL XSTOP                                                        00511300
      STOP                                                              00511400
C                                                                       00511500
C     M IS POINTING AT THE FIRST NON-LETTER AFTER NAME. LOOK FOR        00511600
C     POSSIBLE NAME QUALIFIER OR ARGUMENTS OR END OF CARD.              00511700
C                                                                       00511800
  89  K=KARD(M)                                                         00511900
      IF(K.LT.36)IF(K-10)100,90,90                                      00512000
      IF(K.EQ.40)GO TO 100                                              00512100
      IF(K.EQ.46)GO TO 200                                              00512200
      M=M+1                                                             00512300
      GO TO 89                                                          00512400
C                                                                       00512500
C     A LETTER FOUND, ASSEMBLE SECOND NAME (COMMAND QUALIFIER).         00512600
C                                                                       00512700
  90  CALL NNAME(NAME(3))                                               00512800
C                                                                       00512900
C     CHECK SPECIAL CASE OF NAMES M(XAX"), M(X"AX), M(XX"), M(X"X)      00513000
C                                                                       00513100
C     SKIP ONE CHARACTER (") IF FIRST NAME =(M  )                       00513200
      IF( NAME(1) .EQ. 9477 ) M = M + 1                                 00513300
      GO TO 100                                                         00513400
C                                                                       00513500
C     SCAN FOR ARGUMENTS AND END OF CARD                                00513600
C                                                                       00513700
  98  M=3                                                               00513800
 100  J=J+1                                                             00513900
      GO TO 102                                                         00514000
 101  M=M+1                                                             00514100
 102  K=KARD(M)                                                         00514200
      IF(K.GE.10)IF(K-40)101,120,199                                    00514300
C                                                                       00514400
C     NUMBER FOUND, CONVERT ARGUMENT. IF KARG RETURNED = 0, NUMBER IS   00514500
C     INTEGER,IF KARG = 1, NUMBER IS FLOATING POINT, IF KARG = -1, ERROR00514600
C                                                                       00514700
      CALL AARGS                                                        00514800
      IF(KARG)50,105,103                                                00514900
 103  ARGTAB(J)=0.                                                      00515000
      J=J+1                                                             00515100
      GO TO 110                                                         00515200
C                                                                       00515300
C     ARGUMENT IS AN INTEGER. ADD A BIAS OF 8192 THEN CHECK THAT IT IS  00515400
C     .GT. 0                                                            00515500
C                                                                       00515600
 105  ARG=ARG+8192.                                                     00515700
      IF(ARG.GT.0.)GO TO 110                                            00515800
      CALL ERROR(18)                                                    00515900
      GO TO 50                                                          00516000
 110  ARGTAB(J)=ARG                                                     00516100
  115 NARGS = NARGS + 1                                                 00516200
      GO TO 100                                                         00516300
C                                                                       00516400
C     ASTERISK FOUND, CONVERT                                           00516500
C                                                                       00516600
C     IF BRACKETED BY SINGLE ASTERISKS, QUANTITY IS TO BE USED AS A     00516700
C     FLOATING POINT ARGUMENT.IF BRACKETED BY DOUBLE ASTERISKS, QUANTITY00516800
C     IS TO BE TRUNCATED AND USED AS AN INTEGER ARGUMENT.               00516900
C                                                                       00517000
 120  KARG=1                                                            00517100
      M=M+1                                                             00517200
      IF(KARD(M).NE.40)GO TO 125                                        00517300
      KARG=0                                                            00517400
      M=M+1                                                             00517500
 125  CALL ASTER                                                        00517600
C                                                                       00517700
C     THE TERMINAL ASTERISK(S) HAVE BEEN CHECKED TO BE THE SAME AS THE  00517800
C     INTITAL SET (IF NO ERROR) AND M IS POINTING AT THE FIRST CHARACTER00517900
C     AFTER THE LAST ASTERISK.                                          00518000
C                                                                       00518100
C     KARG RETURNED AS 1 = ERROR FOUND                                  00518200
C                      2 = FLOATING POINT CONSTANT, Z.B.  *PI*          00518300
C                      3 = INTEGER NAMED VARIABLE,  Z.B. **NRMAX**      00518400
C                      4 = FL. PT. NAMED VARIABLE,  Z.B.  *NRMAX*       00518500
C                      5 = INTEGER ROW-COLUMN,      Z.B. **3,40**       00518600
C                      6 = FL. PT. ROW-COLUMN,      Z.B.  *1,2*         00518700
C                      7 = STRING OF ASTERISKS      Z.B. ***            00518800
C                                                                       00518900
C     A STRING OF THREE OR MORE ASTERISKS IMPLIES -THRU-                00519000
C     EXAMPLE..                                                         00519100
C     ERASE 1 2 3 4 12 13 14 15 16 20     IS EQUIVALENT TO              00519200
C     ERASE 1 *** 4, 12 *** 16, 20                                      00519300
C                                                                       00519400
C     PRINT 1 20 19 18 17 16 15 14          IS EQUIVALENT TO            00519500
C     PRINT 1, 20 *** 14                                                00519600
C                                                                       00519700
C                                                                       00519800
      GO TO ( 50, 103, 135, 135, 140, 140, 150 ), KARG                  00519900
 135  ARGTAB(J)=-2.*ARG-FLOAT(KARG-3)                                   00520000
      GO TO 115                                                         00520100
 140  ARGTAB(J)=-(ARG+8208.)                                            00520200
      ARG2=ARG2+8192.                                                   00520300
      IF(KARG.EQ.6)ARG2=-ARG2                                           00520400
      J=J+1                                                             00520500
      ARGTAB(J)=ARG2                                                    00520600
      GO TO 115                                                         00520700
 150  IF( J .GT. 0 ) GO TO 155                                          00520800
      CALL ERROR( 211 )                                                 00520900
      GO TO 102                                                         00521000
 155  ARGTAB( J ) = -1.                                                 00521100
      GO TO 100                                                         00521200
C                                                                       00521300
C                                                                       00521400
C                            ARGTAB SETUP                               00521500
C                                                                       00521600
C     IF ENTRY .GT. 0, IT IS AN INTEGER CONSTANT (Z.B. COLUMN NUMBER)   00521700
C     TO WHICH A BIAS OF 8192 HAS BEEN ADDED.  THIS IS TO SAY THAT A    00521800
C     NEGATIVE INTEGER ARGUMENT MAY NOT BE EXPLICITLY GIVEN OR MODIFIED 00521900
C     TO BE LESS THAT -8191.                                            00522000
C                                                                       00522100
C     IF ENTRY .EQ.0, THE NEXT ENTRY IS A FLOATING POINT CONSTANT.      00522200
C                                                                       00522300
C     IF ENTRY .LT. 0, ARGUMENT IS A VARIABLE. SET SIGN POSITIVE AND..  00522400
C                                                                       00522500
C         IF ENTRY .LT. 16, IT IS A NAMED VARIABLE REFERENCE NUMBER     00522600
C                                                                       00522700
C             2,3  NRMAX      6,7    V       10,11    X                 00522800
C         IF  4,5  COLTOP     8,9    W       12,13    Y                 00522900
C                                            14,15    Z                 00523000
C                                                                       00523100
C                                                                       00523200
C             V,W,X,Y,Z, ARE FOR PROGRAMMING CONVENIENCE ONLY AND DO NOT00523300
C             AFFECT THE OPERATION OF OMNITAB                           00523400
C                                                                       00523500
C             IF ENTRY IS EVEN, CURRENT VALUE TO BE TRUNCATED AND USED  00523600
C             AS AN INTEGER ARGUMENT.                                   00523700
C             IF ENTRY IS ODD. THE CURRENT VALUE IS TO BE USED AS A     00523800
C             FLOATING POINT ARGUMENT.                                  00523900
C                                                                       00524000
C         IF ENTRY .GT. 16, IT IS A WORKSHEET REFERENCE (ROW,COLUMN) TO 00524100
C                  WHICH A BIAS OF 8192. HAS BEEN ADDED.                00524200
C             ENTRY - 8208 = ROW NUMBER                                 00524300
C             ABS(NEXT ENTRY) = COLUMN NUMBER TO WHICH A BIAS OF 8192.  00524400
C                  HAS BEEN ADDED.                                      00524500
C                                                                       00524600
C             IF NEXT ENTRY IS NEGATIVE, WORKSHEET CONTENTS ARE TO BE   00524700
C             USED AS A FLOATING POINT CONSTANT.  IF +, WORKSHEET VALUE 00524800
C             TO BE TRUNCATED AND USED AS AN INTEGER ARGUMENT.          00524900
C                                                                       00525000
C                                                                       00525100
 199  IF(K.NE.46)GO TO 101                                              00525200
C                                                                       00525300
C     END OF CARD FOUND ( $ ENCOUNTERED)                                00525400
C  69  32C                                         2 19 68              00525500
C                                                                       00525600
 200  IF(J.EQ.0)J=1                                                     00525700
      IF(MODE.NE.2.OR.NAME(1).NE.0)GO TO 210                            00525800
C                                                                       00525900
C     IN INPUT MODE AND NO POSSIBLE NAME, RETURN TO SET OR READ ROUTINE 00526000
C                                                                       00526100
 202  CALL EXPAND( J, ARGTAB )                                          00526200
      IF( ISRFLG .EQ. 0 ) GO TO 204                                     00526300
      CALL SETQ                                                         00526400
      GO TO 50                                                          00526500
 204  CALL READQ                                                        00526600
      GO TO 50                                                          00526700
C                                                                       00526800
C     LOOK UP NAME (AND POSSIBLE QUALIFIER) IN DICTIONARY. RETURN       00526900
C     COORDINATES OF ENTRY. IF L1 = 0, NAME NOT FOUND                   00527000
C                                                                       00527100
 210  CALL LOOKUP                                                       00527200
      IF(L1.NE.0)GO TO 220                                              00527300
      IF(MODE.EQ.2)GO TO 202                                            00527400
      CALL ERROR(1)                                                     00527500
      GO TO 50                                                          00527600
C                                                                       00527700
C     NAME FOUND                                                        00527800
C                                                                       00527900
 220  IF(MODE.EQ.2)MODE=1                                               00528000
      IF(MODE.EQ.1)GO TO 222                                            00528100
      CALL STORE(J)                                                     00528200
      GO TO 50                                                          00528300
 222  CALL EXPAND( J, ARGTAB )                                          00528400
      CALL XECUTE                                                       00528500
      GO TO 50                                                          00528600
      END                                                               00528700
C  70 477      SUBROUTINE ORTHO                    2 19 68              00528800
      SUBROUTINE ORTHO                                                  00528900
C                                                                       00529000
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00529100
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00529200
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00529300
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00529400
      DIMENSION ARGS(100)                                               00529500
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00529600
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00529700
      COMMON / SCRAT / SCRA(10000),NS                                   00529800
C *******       *******       *******                                   00529900
C   ORTHONORMALIZATION PROGRAM  BY PHILIP J. WALSH  JULY 1, 1967        00530000
C   LEAST SQUARES PROGRAM USING GRAM SCHMIDT PROCESS                    00530100
C                                                                       00530200
C   POLYFIT  Y IN ++, WEIGHTS IN ++,  X IN ++,  DEGREE ++, STORE COEF.IN00530300
C                 ++  AND DEVIATIONS IN ++                              00530400
C                                                                       00530500
C   FIT  Y IN ++,  WEIGHTS IN ++, X IN ++, X IN ++, ..., STORE COEF. IN 00530600
C                 ++  AND DEVIATIONS IN ++                              00530700
C                                                                       00530800
C   SOLVE  B IN ++, USING COEF. MATRIX IN ++, ++, ..., ++, STORE COEF IN00530900
C                 ++ AND  DEVIATIONS IN ++                              00531000
C                                                                       00531100
C                                                                       00531200
C                                                                       00531300
C                                                                       00531400
      IX1 (I,J,IN) = IN + I * (I-1)/2 + J                               00531500
      IX2 (I,J,IP) = IP + I * (I-1)/2 + J                               00531600
C     PRECHECKING SECTION                                               00531700
      IF(NARGS .GT. 0 ) GO TO 10000                                     00531800
      K = 10                                                            00531900
      GO TO 10001                                                       00532000
10000 NRBAR = 1                                                         00532100
      N = NRMAX                                                         00532200
      FN = N                                                            00532300
C     GET Y COLUMN (OR B COLUMN IF SOLVE)                               00532400
      CALL ADRESS( 1,L11)                                               00532500
      IF ( L11 ) 9000, 9000, 9010                                       00532600
 9000 K = 11                                                            00532700
10001 CALL ERROR( K )                                                   00532800
      RETURN                                                            00532900
 9010 CALL ADRESS(NARGS,L66)                                            00533000
      CALL ADRESS(NARGS-1,L55)                                          00533100
      IF (L66) 9000,9000, 8000                                          00533200
 8000 IF (L55) 9000,9000, 8010                                          00533300
 8010 L66 = L66 - 1                                                     00533400
      L55 = L55 -1                                                      00533500
      GO TO ( 9020, 9020, 9020, 9020, 9030, 9030) , L2                  00533600
C     GET WEIGHTS (IF NOT SOLVE )                                       00533700
 9020 CALL ADRESS( 2, L22)                                              00533800
      IF ( L22)  9000, 9000, 9040                                       00533900
 9040 NMUI = 2                                                          00534000
      L22 = L22 - 1                                                     00534100
      SU = 0.                                                           00534200
      DO 9100 I = 1, NRMAX                                              00534300
      IF (RC( I + L22 ) ) 9110,9100, 9110                               00534400
 9110 SU = SU + 1.0                                                     00534500
 9100 CONTINUE                                                          00534600
      GO TO (9050,9050,9060,9060), L2                                   00534700
C     THIS IS POLYFIT                                                   00534800
 9050 IF( NARGS .NE. 6) GO TO 9000                                      00534900
      M = IARGS(4) + 1                                                  00535000
      GO TO 9120                                                        00535100
C     THIS IS FIT                                                       00535200
 9060 M = NARGS - 4                                                     00535300
      IF (NARGS - 5) 9000, 9120, 9120                                   00535400
C     THIS IS SOLVE                                                     00535500
 9030 M = NARGS - 3                                                     00535600
      DENOM = 1.0                                                       00535700
      NMUI = 1                                                          00535800
      IF ( NARGS - 4) 9000, 9070, 9070                                  00535900
C     Y IN L11 W IN L22  AND WEIGHT SWITCH SET AT THIS POINT            00536000
C                                                                       00536100
C     CALCULATE SCRATCH AREA REQUIREMENTS ...                           00536200
 9120 FM = M                                                            00536300
      IF (FN - SU ) 9000, 9140, 9150                                    00536400
 9150 IF (SU - FM ) 9000, 9160, 9140                                    00536500
 9140 DENOM = FSQRT( SU - FM )                                          00536600
      GO TO 9070                                                        00536700
 9160 DENOM = 1.0                                                       00536800
 9070 NPM = N + M                                                       00536900
      M1 = M - 1                                                        00537000
      M2 = M + 1                                                        00537100
      N1 = N - 1                                                        00537200
      N2 = N + 1                                                        00537300
      MD1 = ( M * (M2)) / 2                                             00537400
C                                                                       00537500
C                                                                       00537600
      ND1 = M2 * NPM                                                    00537700
C                                                                       00537800
C     X REQUIRES ND1 CELLS                                              00537900
C     GET SCRA (ND1 + 1) FOR START OF PK                                00538000
      ND2 = M*NPM                                                       00538100
      MD3 = ND2 + N                                                     00538200
      ND3 = ND1                                                         00538300
C     ADD NPM TO REACH XP                                               00538400
      ND4 = ND3 + NPM                                                   00538500
C     ADD NPM TO REACH QK                                               00538600
      ND5 = ND4 + NPM                                                   00538700
C     ADD (M+1) TO REACH CV                                             00538800
      ND6 = ND5 + M2                                                    00538900
C     ADD (M*(M+1))/2 + M  TO REACH VCV                                 00539000
      ND66 = MD1 + M                                                    00539100
      ND7  = ND6 + ND66                                                 00539200
C   ADD THE SAME AMOUNT TO REACH Q                                      00539300
      ND8 = ND7 + ND66                                                  00539400
C     Q IS (M+1) CELLS LONG  THEN COMES Q2                              00539500
      ND9 = ND8 + M2                                                    00539600
C Q2 E AND EP ARE EACH M CELLS LONG                                     00539700
      ND10 = ND9 + M                                                    00539800
      ND11 = ND10 + M                                                   00539900
      ND12 = ND11 + M                                                   00540000
C THE A MATRIX IS NEXT                                                  00540100
      ND13 = ND12 + MD1                                                 00540200
C GRAM FACTOR STORAGE                                                   00540300
      ND14 = ND13 + M2                                                  00540400
C  ENF                                                                  00540500
C CV DIAGONALS                                                          00540600
      ND16 = ND14 + M                                                   00540700
C VCV DIAGONALS                                                         00540800
      ND17 = ND16 + M                                                   00540900
      ND18 = ND17 + M                                                   00541000
      IF( ND18 - 10000 ) 9090,9090,9000                                 00541100
C                                                                       00541200
 9090 IF( NERROR .NE. 0 ) RETURN                                        00541300
      GO TO ( 1, 1, 2, 2, 3, 3 ), L2                                    00541400
C    THIS IS POLYFIT                                                    00541500
    1 CALL ADRESS(3,L33)                                                00541600
      IF (L33) 9000, 9000, 11                                           00541700
   11 L33 = L33 - 1                                                     00541800
      DO 21 I = 1, N                                                    00541900
      K = I + NPM                                                       00542000
      SCRA( I) = 1.0                                                    00542100
   21 SCRA(K) = RC(I + L33)                                             00542200
      IF ( M .EQ. 2 ) GO TO 100                                         00542300
      DO 41 K = 2, M1                                                   00542400
      K2 = K* NPM                                                       00542500
      K1 = K2 - NPM                                                     00542600
      DO 31 I = 1, N                                                    00542700
      I2 = I + K2                                                       00542800
      I1 = I + K1                                                       00542900
   31 SCRA(I2) = SCRA(I1) * RC(I + L33)                                 00543000
   41 CONTINUE                                                          00543100
      GO TO 100                                                         00543200
    2 I = 3                                                             00543300
      GO TO 14                                                          00543400
    3 I = 2                                                             00543500
   14 L44 = NARGS - 2                                                   00543600
      J = 0                                                             00543700
      DO 44 I1 = I,L44                                                  00543800
      K1 = J * NPM                                                      00543900
      CALL ADRESS(I1, L33)                                              00544000
      IF (L33) 9000, 9000, 24                                           00544100
   24 L33 = L33 - 1                                                     00544200
      DO 54 I2 = 1, N                                                   00544300
      K2 = K1 + I2                                                      00544400
   54 SCRA(K2) = RC(I2 + L33)                                           00544500
   44 J = J + 1                                                         00544600
C     GENERATE IDENTITY MATRIX AUGMENTATION                             00544700
  100 DO 120  K = 1, M                                                  00544800
      K1 = (K - 1) * NPM + N                                            00544900
      DO 110  I = 1, M                                                  00545000
      K2  = K1 + I                                                      00545100
  110 SCRA( K2) = 0.0                                                   00545200
      K2 = K1 + K                                                       00545300
  120 SCRA(K2) = 1.0                                                    00545400
C     BEGIN THE G.S. PROCESS                                            00545500
  200 NBEI = 1                                                          00545600
      NRHI = 1                                                          00545700
      I18  = 1                                                          00545800
  210 NGAI = 2                                                          00545900
      NSII = 2                                                          00546000
  230 NDEI = 1                                                          00546100
      NNUI = 1                                                          00546200
      LZ1  = 1                                                          00546300
      LZ2  = 1                                                          00546400
C  K CONTROLS WHOLE  LOOP                                               00546500
      K = 1                                                             00546600
  240 NTHI = 1                                                          00546700
  250 NALI = 1                                                          00546800
      NOMI = 1                                                          00546900
  260 DO  270  J = 1, M                                                 00547000
      NJ = ND3 + N + J                                                  00547100
  270 SCRA (NJ) = 0.0                                                   00547200
C****    BOX 6.                                                         00547300
  390 KD1 =(K-1)* NPM                                                   00547400
      DO  300  I = 1, N                                                 00547500
      I1 = ND3 + I                                                      00547600
      I2 = KD1 + I                                                      00547700
      GO TO ( 280, 290), NMUI                                           00547800
C****    PK(I)                                                          00547900
  280 SCRA( I1) =  SCRA (I2)                                            00548000
      GO  TO 300                                                        00548100
  290 SCRA (I1) =  SCRA (I2) * RC ( I + L22)                            00548200
  300 CONTINUE                                                          00548300
  310 GO TO ( 320, 330), NOMI                                           00548400
  320 DO  340 I = 1, K                                                  00548500
      I1 = (I-1)*NPM                                                    00548600
      SUM = 0.0                                                         00548700
      DO  350 J = 1, NPM                                                00548800
      I2 = I1 + J                                                       00548900
      J2 = J + ND3                                                      00549000
  350 SUM = SUM + SCRA(J2) * SCRA(I2)                                   00549100
      I2 = I + ND5                                                      00549200
C****     QK(I)                                                         00549300
  340 SCRA(I2) = SUM                                                    00549400
      GO TO 360                                                         00549500
  330 DK2 = 0.                                                          00549600
      I1 =(K-1) * NPM                                                   00549700
      DO  370 I = 1, NPM                                                00549800
  370 DK2 = DK2 +  SCRA ( I + ND3) * SCRA (I + I1)                      00549900
      DK = FSQRT( DK2 )                                                 00550000
C****    GRAM FACTORS                                                   00550100
      SCRA ( I18 + ND13) = DK                                           00550200
      I18 = I18 + 1                                                     00550300
      K1 = (K-1)* NPM                                                   00550400
      DO  380  I = 1, NPM                                               00550500
  380 SCRA  (I + K1) = SCRA (I + K1) / DK                               00550600
      NOMI  = 1                                                         00550700
      GO TO 390                                                         00550800
C****    BOX8                                                           00550900
  360 GO TO ( 400, 410), NDEI                                           00551000
  400 LZ1 = -LZ1                                                        00551100
      IF (LZ1)  420, 430, 430                                           00551200
C****    BOX8A                                                          00551300
  430 K1 = K- 1                                                         00551400
      DO  440  I = 1, K1                                                00551500
  440 SCRA (I+ND5) =  - SCRA(I +ND5)                                    00551600
      SCRA (K+ND5) = 1.0                                                00551700
      DO  450  I = 1, NPM                                               00551800
      SUM = 0.0                                                         00551900
      DO  460  J = 1, K                                                 00552000
      J1 = (J-1)* NPM                                                   00552100
  460 SUM = SUM +  SCRA (I +J1) * SCRA (J+ND5 )                         00552200
C****    XP(I)                                                          00552300
  450 SCRA (I + ND4 ) = SUM                                             00552400
      GO TO  470                                                        00552500
C****    BOX8B    GET QK(I18)                                           00552600
  420 SCRA(ND14+I18)=FSQRT(SCRA(ND5+K))                                 00552700
      GO TO 430                                                         00552800
C****    NDE1                                                           00552900
  410 LZ2 = -LZ2                                                        00553000
      IF (LZ2) 480, 430, 430                                            00553100
C****    GET E AMD OTHER VECTORS                                        00553200
  480 DO  490 I = 1, M                                                  00553300
      SCRA (I + ND8) = SCRA( I + ND5)                                   00553400
  490 SCRA (I + ND9) = SCRA( I + ND5) * SCRA (I + ND5)                  00553500
      SCRA ( ND8 + M2) = SCRA (ND5 + M2)                                00553600
      SCRA ( ND10 + 1) = SCRA ( ND8 +M2) -  SCRA(ND9 + 1)               00553700
      DO  500  J = 2, M                                                 00553800
      J1 = J - 1                                                        00553900
  500 SCRA (ND10 + J) =  SCRA (ND10 + J1) - SCRA( ND9 + J)              00554000
      FI = 1.0                                                          00554100
      DO  510  I = 1, M                                                 00554200
      IF (FN - FI)   520,520, 530                                       00554300
  530 IF ( SCRA ( ND10 +I)) 540,550,550                                 00554400
  540 SCRA (ND11 + I)  = - SQRT ( ABS ( SCRA ( I+ND10))/(FN-FI))        00554500
      GO TO 510                                                         00554600
  550 SCRA(I + ND11) = SQRT(SCRA(I + ND10)/(FN-FI))                     00554700
      GO TO 510                                                         00554800
  520 SCRA (I+ND10) = -1.0                                              00554900
  510 FI = FI + 1.0                                                     00555000
      GO TO 430                                                         00555100
C****    BOX9                                                           00555200
  470 GO TO (610,620,630), NTHI                                         00555300
  610 K1 = (K-1)* NPM                                                   00555400
      DO  640 I= 1, NPM                                                 00555500
  640 SCRA ( I + K1) =  SCRA (I + ND4)                                  00555600
      GO TO 800                                                         00555700
  620 DO  650 I = 1, N                                                  00555800
  650 RC( I + L66) =  SCRA ( I + ND4)                                   00555900
      DO  660 I = 1, M                                                  00556000
      NI = N+I                                                          00556100
      KK1= I+1                                                          00556200
  660 RC(L55+KK1) =  - SCRA( NI + ND4)                                  00556300
      NTHI = 3                                                          00556400
      GO TO 610                                                         00556500
  630 GO TO 900                                                         00556600
C****    BOX10                                                          00556700
  800 GO TO (810, 830), NALI                                            00556800
  810 NOMI = 2                                                          00556900
      NALI = 2                                                          00557000
      GO TO 390                                                         00557100
  830 IF  (K - M) 820, 1000, 1000                                       00557200
  820 K= K+1                                                            00557300
      GO TO 240                                                         00557400
  900 GO TO (910,920), NNUI                                             00557500
  910 NNUI = 2                                                          00557600
      GO TO  1200                                                       00557700
  920 SS =  DK / DENOM                                                  00557800
      SSQ = SS*SS                                                       00557900
  930 RC ( L55 +1) = SS                                                 00558000
      GO TO 1200                                                        00558100
 1000 GO TO (1010,1020), NBEI                                           00558200
C**  ******    ***** ****** ***** ****** *****                          00558300
C GET THE A MATRIX                                                      00558400
 1010 K1 = 1                                                            00558500
      DO 1060 I = 1, M                                                  00558600
      I1 = I*N + (I-1)*M                                                00558700
      DO 1070 J = 1, I                                                  00558800
      I2 = J + I1                                                       00558900
      K2 = K1 + ND12                                                    00559000
      SCRA(K2) = SCRA (I2)                                              00559100
 1070 K1 = K1+ 1                                                        00559200
 1060 CONTINUE                                                          00559300
      GMDT = 1.0                                                        00559400
      DO 1080 I = 1, M                                                  00559500
 1080 GMDT = GMDT *(SCRA (I + ND13)/ SCRA(I +ND14)   )                  00559600
      GMDT = GMDT * GMDT                                                00559700
C                                                                       00559800
      NDEI = 2                                                          00559900
      NBEI = 2                                                          00560000
      NTHI = 2                                                          00560100
      K = K + 1                                                         00560200
      GO TO 1030                                                        00560300
 1020 GO TO 900                                                         00560400
 1030 GO TO (1040,1050),NGAI                                            00560500
 1040 GO TO 900                                                         00560600
C GET CV MATRIC                                                         00560700
 1050 CONTINUE                                                          00560800
      DO  111  IH = 1, M                                                00560900
      LOC = IX2 (IH,0, ND6)                                             00561000
      DO  111  J = 1, IH                                                00561100
      SUM = 0.                                                          00561200
      DO  112  KK = IH, M                                               00561300
      LOC1 = IX1( KK, IH, ND12)                                         00561400
      LOC2 = IX1( KK, J, ND12)                                          00561500
 112  SUM = SUM + SCRA (LOC1)* SCRA(LOC2)                               00561600
 111  SCRA ( LOC + J) = SUM                                             00561700
      J = 1                                                             00561800
      SCRA(J+ND16)=FSQRT(SCRA(J+ND6))                                   00561900
      DO 2050 I = 2, M                                                  00562000
      J1 = I + J + ND6                                                  00562100
C**           THE ARGUMENT IN THE FOLLOWING SQRT OCCASIONALLY IS NEGATIV00562200
      SCRA(I+ND16)=FSQRT(SCRA(J1))                                      00562300
 2050 J = J + I                                                         00562400
      NGAI = 1                                                          00562500
      GO TO 900                                                         00562600
 1200 GO TO (1210,1220),NRHI                                            00562700
 1210 IF(NRBAR) 1230,1500,1230                                          00562800
 1230 NRBAR = NRBAR - 1                                                 00562900
      NTHI = 2                                                          00563000
      NRHI = 2                                                          00563100
      L11 = L11 - 1                                                     00563200
      DO 1240 I = 1,N                                                   00563300
      I1 = I + ND2                                                      00563400
 1240 SCRA(I1) = RC(I+L11)                                              00563500
      DO 1250 I = 1, M                                                  00563600
      I1 = I + MD3                                                      00563700
 1250 SCRA(I1) = 0.                                                     00563800
      GO TO 250                                                         00563900
 1220 GO TO (1410,1420), NSII                                           00564000
 1410 GO TO 1210                                                        00564100
C GET VCV AND DEV AND COEF                                              00564200
 1420 DO 1421 I = 1,MD1                                                 00564300
 1421 SCRA(I + ND7) = SSQ* SCRA(I + ND6)                                00564400
      DO 2070 I = 1,M                                                   00564500
      J1 = I + L55 + M2                                                 00564600
      SCRA(I+ND17)= SS*SCRA(I+ND16)                                     00564700
 2070 RC(J1) = SCRA(I+ND17)                                             00564800
      GO TO 1210                                                        00564900
C THE CALCULATIONS ARE COMPLETED. NOW OUTPUT THE RESULTS                00565000
 1500 GO TO ( 2000, 5000, 2000, 5000, 2000, 5000), L2                   00565100
 5000 RETURN                                                            00565200
 2000 CALL PAGE (4)                                                     00565300
C GET POLYFIT INFORMATION                                               00565400
      IF ( L2 - 3 )  2010,2020,2100                                     00565500
C THIS IS POLYFIT                                                       00565600
 2010 WRITE(IPRINT,2001) IARGS(4),IARGS(1),IARGS(3)                     00565700
      KL1 = IARGS( 4 ) + 1                                              00565800
      DO 2700 I=1,KL1                                                   00565900
 2700 IARGS(I+2) = I-1                                                  00566000
      GO TO 2030                                                        00566100
C THIS IS FIT                                                           00566200
 2020 J=NARGS-2                                                         00566300
      WRITE(IPRINT,2021) (IARGS(I),I=1,J)                               00566400
 2030 I = SU                                                            00566500
      WRITE(IPRINT,2031) I,IARGS(2)                                     00566600
      IF ( L2 .NE. 3 ) GO TO 2701                                       00566700
      WRITE(IPRINT,2032)                                                00566800
      GO TO 2702                                                        00566900
 2701 WRITE(IPRINT,2732)                                                00567000
 2702 L55 = L55+1                                                       00567100
      DO 2038 I = 1,M                                                   00567200
      J = I + M + L55                                                   00567300
      WRITE(IPRINT,2235) IARGS(I+2),RC(I+L55),RC(J)                     00567400
 2038 CONTINUE                                                          00567500
      WRITE(IPRINT,2033) SS                                             00567600
      WRITE(IPRINT,2034)                                                00567700
      DO2040 I = 1, M                                                   00567800
      LOC = IX2(I,0,ND6)                                                00567900
 2040 WRITE(IPRINT,2035) (SCRA(J+LOC),J=1,I)                            00568000
      WRITE(IPRINT,2036)                                                00568100
      WRITE(IPRINT,2035) (SCRA(I+ND16),I=1,M)                           00568200
C REPEAT THIS FOR  VARIANCE COVARIANCE MATRIX                           00568300
      WRITE(IPRINT,2037)                                                00568400
      DO 2060  I = 1, M                                                 00568500
      LOC = IX2(I,0,ND7)                                                00568600
 2060 WRITE(IPRINT,2035) (SCRA(J+LOC),J=1,I)                            00568700
      WRITE(IPRINT,2071)                                                00568800
      WRITE(IPRINT,2035) (SCRA(I+ND17),I=1,M)                           00568900
      WRITE(IPRINT,2082)                                                00569000
      DO 2083 I = 1,M                                                   00569100
      LOC = IX2(I,0,ND12)                                               00569200
 2083 WRITE(IPRINT,2035) (SCRA(J+LOC),J=1,I)                            00569300
      CALL PAGE (4)                                                     00569400
      WRITE(IPRINT,2081) GMDT                                           00569500
      WRITE(IPRINT,2080)                                                00569600
      DO  2090  I =  1,  M                                              00569700
 2090 WRITE(IPRINT,2035) SCRA(I+ND13),SCRA(I+ND8),SCRA(I+ND9)           00569800
     1 , SCRA (I + ND10), SCRA (I + ND11), SCRA (I + ND14)              00569900
      WRITE(IPRINT,2035)SCRA(ND14),SCRA(ND9)                            00570000
      WRITE(IPRINT,2133)                                                00570100
      IDF=SU                                                            00570200
      XSQR=SCRA(ND9)/SU                                                 00570300
      JGO = 2                                                           00570400
      IF ( L2 .EQ. 1 ) JGO = 1                                          00570500
      WRITE(IPRINT,2134) SCRA(ND9),IDF,XSQR                             00570600
      DO 2091 I=1,M                                                     00570700
      IDF = SU-FLOAT(I)                                                 00570800
      XSQR=SCRA(I+ND9)                                                  00570900
      XSQE=SCRA(I+ND10)/FLOAT(IDF)                                      00571000
      FCALC=XSQR/XSQE                                                   00571100
      JONE=1                                                            00571200
      GO TO (2781,2782),JGO                                             00571300
 2782 WRITE(IPRINT,2135) IARGS(I+2),SCRA(I+ND9),JONE,XSQR,FCALC,SCRA(I +00571400
     1 ND10),IDF,     XSQE,SCRA(I+ND11)                                 00571500
      GO TO 2091                                                        00571600
 2781 WRITE(IPRINT,2735) IARGS(I+2),SCRA(I+ND9),JONE,XSQR,FCALC,SCRA(I +00571700
     1 ND10),IDF,     XSQE,SCRA(I+ND11)                                 00571800
 2091 CONTINUE                                                          00571900
      XSQ=SCRA(ND9)-SCRA(ND10+M)                                        00572000
      XSQE=XSQ/FLOAT(M)                                                 00572100
      FCALC=XSQE/(SCRA(M+ND10)/(SU-FLOAT(M)))                           00572200
      WRITE(IPRINT,2136)XSQ,M,XSQE,FCALC                                00572300
      GO TO 5000                                                        00572400
 2100 J = NARGS - 2                                                     00572500
      WRITE(IPRINT,2110) (IARGS(I),I=1,J)                               00572600
      WRITE(IPRINT,2115)                                                00572700
      L55 = L55 + 1                                                     00572800
      DO 2120 I = 1,M                                                   00572900
      J = I + M + L55                                                   00573000
      WRITE(IPRINT,2035) RC(I+L55),RC(J)                                00573100
 2120 CONTINUE                                                          00573200
      GO TO 5000                                                        00573300
 2001 FORMAT(25H0POLYNOMIAL FIT OF DEGREE I3,19H TO THE FUNCTION IN I4, 00573400
     1                  1H.//42H THE INDEPENDENT VARIABLE (X) IS IN COLU00573500
     1MN  I4,1H.)                                                       00573600
 2021 FORMAT(34H0REGRESSION FIT OF THE FUNCTION IN I4,11H,WEIGHTS IN I4,00573700
     127H,USING VARIABLES IN COLUMNS// (14I5) )                         00573800
 2032 FORMAT(62H0VARIABLE IN COLUMN     COEFFICIENT AND ITS STANDARD DEV00573900
     1IATION,/)                                                         00574000
 2732 FORMAT(62H0TERM OF DEGREE         COEFFICIENT AND ITS STANDARD DEV00574100
     1IATION,/)                                                         00574200
 2031 FORMAT(1H0,I4,34H NON-ZERO WEIGHTS APPEAR IN COLUMN I4,1H.)       00574300
 2033 FORMAT(20H0STANDARD DEVIATION 1P1E15.7)                           00574400
 2035 FORMAT(1P8E15.7)                                                  00574500
 2034 FORMAT(54H0THE INVERSE OF THE X"X MATRIX OF THE NORMAL EQUATIONS/)00574600
 2036 FORMAT(53H0THE SQUARE ROOT OF THE DIAGONALS OF THE ABOVE MATRIX/) 00574700
 2037 FORMAT(50H0THE VARIANCE-COVARIANCE MATRIX OF THE REGRESSION ,     00574800
     112HCOEFFICIENTS/)                                                 00574900
 2071 FORMAT(67H0THE SQUARE ROOT OF THE DIAGONALS IN THE VARIANCE-COVARI00575000
     1ANCE MATRIX / )                                                   00575100
 2080 FORMAT( 1H0,5X,2HGF,14X,2HFC,13X,3HSFC,13X,3HSSR,13X,1HR,         00575200
     1     13X,2HVN /)                                                  00575300
 2081 FORMAT(20H0GRAM DETERMINANT   1P1E15.7)                           00575400
 2082 FORMAT(20H0THE A(I,J) MATRIX   /)                                 00575500
 2110  FORMAT(36H0SOLUTION OBTAINED BY FIT TO COLUMN   I3,/30H USING COE00575600
     1FFICIENTS IN COLUMNS   /(14I5))                                   00575700
 2115 FORMAT(28H0SOLUTION AND UNCERTAINTIES    /)                       00575800
 2133 FORMAT(//30X,21H ANALYSIS OF VARIANCE,/  7H0SOURCE,17X,15HSUM OF S00575900
     1QUARES , 7H   D.F.,6X,11HMEAN SQUARE,10X,1HF,12X,4HS.D.)          00576000
 2134 FORMAT( 6H0TOTAL,18X,1PE15.7,I6,4X,1PE15.7  )                     00576100
 2135 FORMAT(19H0VARIABLE IN COLUMN,I5,1PE15.7,I6,4X,1PE15.7,0PF11.2,/  00576200
     1  9H RESIDUAL,15X,1PE15.7,I6,4X,1PE15.7,19X,1PE15.7  )            00576300
 2735 FORMAT(19H0TERM OF DEGREE    ,I5,1PE15.7,I6,4X,1PE15.7,0PF11.2,/  00576400
     1  9H RESIDUAL,15X,1PE15.7,I6,4X,1PE15.7,19X,1PE15.7  )            00576500
 2136 FORMAT(16H0TOTAL REDUCTION,8X,1PE15.7,I6,4X,1PE15.7,F11.2 )       00576600
 2235 FORMAT(14X,I4,7X,2(1PE15.7)  )                                    00576700
      END                                                               00576800
C  71  22      SUBROUTINE OUTPUT                   2 19 68              00576900
      SUBROUTINE OUTPUT                                                 00577000
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00577100
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00577200
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00577300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00577400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00577500
      DIMENSION ARGS(100)                                               00577600
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00577700
      COMMON / FLAGS / NSUMRY, LLIST                                    00577800
C                                                                       00577900
C     WRITE RECORD ON SCRATCH UNIT                                      00578000
C                                                                       00578100
      IF( NERROR .EQ. 0 .AND. LLIST .EQ. 0 ) GO TO 15                   00578200
      IF( MODE .EQ. 3 ) GO TO 20                                        00578300
      WRITE( ISCRAT, 10 ) NEWCD                                         00578400
  10  FORMAT(4X,80A1)                                                   00578500
  15  RETURN                                                            00578600
  20  I = NSTMT / 10                                                    00578700
      WRITE( ISCRAT, 25 ) I, NEWCD                                      00578800
  25  FORMAT(1H+,I3,80A1)                                               00578900
      GO TO 15                                                          00579000
      END                                                               00579100
C  72  18      SUBROUTINE PAGE( J )                2 19 68              00579200
      SUBROUTINE PAGE( J )                                              00579300
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00579400
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00579500
C                                                                       00579600
C         BRING UP A NEW PAGE AND PRINT OMNITAB CARD AND PAGE NUMBER    00579700
C         THEN, IF J = 0, DONE                                          00579800
C                 J = 1, PRINT TITLE1                                   00579900
C                  J = 2, PRINT TITLE1, 2                               00580000
C                    ETC. FOR J = 3, 4                                  00580100
C                                                                       00580200
      NPAGE = NPAGE + 1                                                 00580300
      WRITE( IPRINT, 100 ) NMCARD, NPAGE                                00580400
      IF( J .LE. 0 .OR. J .GT. 4 ) GO TO 10                             00580500
      WRITE( IPRINT, 101 ) ( ( ITLE( I, II ), I = 1, 60 ), II = 1, J )  00580600
  10  RETURN                                                            00580700
 100  FORMAT(1H1,19X,72A1,10X,4HPAGE,I4)                                00580800
 101  FORMAT(1X,120A1/1X,120A1)                                         00580900
      END                                                               00581000
C  73   4      SUBROUTINE PAGEX                    2 19 68              00581100
      SUBROUTINE PAGEX                                                  00581200
      CALL X( "PAGEX" )                                                 00581300
      RETURN                                                            00581400
      END                                                               00581500
C  74  93      SUBROUTINE PDMOTE                   2 19 68              00581600
      SUBROUTINE PDMOTE                                                 00581700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00581800
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00581900
      DIMENSION ARGS(100)                                               00582000
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00582100
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00582200
      L2 = L2 - 10                                                      00582300
C                                                                       00582400
C     L2 " 0 FOR PROMOTE,   1 FOR DEMOTE    ( L2 ORIGINALLY 10, 11 )    00582500
C                                                                       00582600
      IF( MOD( NARGS, 2 ) .NE. 0 ) GO TO 30                             00582700
      I = 10                                                            00582800
  10  CALL ERROR( I )                                                   00582900
  20  RETURN                                                            00583000
  30  NR = IARGS( 1 )                                                   00583100
      IARGS( 1 ) = 1                                                    00583200
      CALL CHKCOL( I )                                                  00583300
      IF( I .EQ. 0 ) GO TO 40                                           00583400
      I = 20                                                            00583500
      GO TO 10                                                          00583600
C                                                                       00583700
C     IF NUMBER OF ROWS TO BE MOVED IS NEGATIVE, FLIP INSTRUCTIONS.     00583800
C     I.E.  PROMOTE -6  IS THE SAME AS   DEMOTE  6   .                  00583900
  40  IF( NR ) 50, 20, 60                                               00584000
  50  L2 = 1 - L2                                                       00584100
      NR = -NR                                                          00584200
  60  NARGS = NARGS - 1                                                 00584300
C                                                                       00584400
C     CHECK DISTANCE OF MOVE                                            00584500
C                                                                       00584600
      IF( L2 .EQ. 0 ) GO TO 80                                          00584700
      IF( NR + NRMAX .LE. NROW ) GO TO 100                              00584800
      CALL ERROR(213)                                                   00584900
      NRMAX = NROW - NR                                                 00585000
      GO TO 100                                                         00585100
  70  I = 205                                                           00585200
      GO TO 10                                                          00585300
  80  IF( NR - NRMAX ) 100, 90, 70                                      00585400
C                                                                       00585500
C     PROMOTE "NRMAX"  ...                                              00585600
C                                                                       00585700
  90  IF( NARGS .NE. 0 ) GO TO 20                                       00585800
      IF( NRMAX .EQ. 0 ) GO TO 70                                       00585900
      J = IARGS( 1 )                                                    00586000
      DO 95 I = 1, NCOL                                                 00586100
      CALL VECTOR( 0., J )                                              00586200
  95  J = J + NROW                                                      00586300
      GO TO 20                                                          00586400
 100  LIMIT = NARGS                                                     00586500
      IF( LIMIT .EQ. 0 ) LIMIT = 2 * NCOL                               00586600
      IF( NERROR .NE. 0 ) GO TO 20                                      00586700
      IF( NRMAX .NE. 0 ) GO TO 110                                      00586800
      GO TO 10                                                          00586900
C                                                                       00587000
C     START PROMOTING OR DEMOTING                                       00587100
C                                                                       00587200
 110  DO 200 I = 1, LIMIT, 2                                            00587300
      IF( NARGS .NE. 0 ) GO TO 120                                      00587400
      K1 = IARGS( 1 )                                                   00587500
      K2 = K1                                                           00587600
      IARGS( 1 ) = IARGS( 1 ) + NROW                                    00587700
      GO TO 130                                                         00587800
 120  K1 = IARGS (I+1 )                                                 00587900
      K2 = IARGS( I+2 )                                                 00588000
 130  IF( L2 .EQ. 0 ) GO TO 150                                         00588100
C                                                                       00588200
C     DEMOTE COL AT K1 TO COL AT K2                                     00588300
C                                                                       00588400
      K1 = K1 + NRMAX                                                   00588500
      K2 = K2 + NRMAX + NR                                              00588600
      DO 140 J = 1, NRMAX                                               00588700
      K1 = K1 - 1                                                       00588800
      K2 = K2 - 1                                                       00588900
 140  RC ( K2 ) = RC( K1 )                                              00589000
      GO TO 200                                                         00589100
C                                                                       00589200
C     PROMOTE COL AT K1 TO COL AT K2                                    00589300
C                                                                       00589400
 150  JJ = NRMAX - NR                                                   00589500
      K1 = K1 + NR                                                      00589600
      DO 160 J = 1, JJ                                                  00589700
      RC( K2 ) = RC( K1 )                                               00589800
      K1 = K1 + 1                                                       00589900
 160  K2 = K2 + 1                                                       00590000
C                                                                       00590100
C     IF PROMOTE ARRAY, FILL REST OF COLUMN WITH ZEROES.                00590200
C                                                                       00590300
      IF( NARGS .NE. 0 ) GO TO 200                                      00590400
      JJ = JJ + 1                                                       00590500
      DO 170 J = JJ, NRMAX                                              00590600
      RC( K2 ) = 0.                                                     00590700
 170  K2 = K2 + 1                                                       00590800
 200  CONTINUE                                                          00590900
      IF( L2 .NE. 0 ) NRMAX = NRMAX + NR                                00591000
      GO TO 20                                                          00591100
      END                                                               00591200
C  75  47      SUBROUTINE PHYCON(NAME)             2 19 68              00591300
      SUBROUTINE PHYCON(NAME)                                           00591400
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00591500
      COMMON / PCONST / P( 40 ), N( 40 )                                00591600
      DATA J/-1/                                                        00591700
C                                                                       00591800
C     PHYSICAL CONSTANT LIST                                            00591900
C                                                                       00592000
C     ENTRIES ARE IN PAIRS, FIRST MKS VALUE, THEN CGS (ELECTROMAGNETIC) 00592100
C                                                                       00592200
C                                                                       00592300
C        PI        PI                                                   00592400
C        E         BASE OF NATURAL LOGS                                 00592500
C        C         SPEED OF LIGHT IN VACUUM                             00592600
C        Q         ELEMENTARY CHARGE                                    00592700
C        N         AVOGADRO CONSTANT                                    00592800
C        ME        ELECTRON REST MASS                                   00592900
C        MP        PROTON REST MASS                                     00593000
C        F         FARADAY CONSTANT                                     00593100
C        H         PLANCK CONSTANT                                      00593200
C        ALPHA     FINE STRUCTURE CONSTANT                              00593300
C        QME       CHARGE TO MASS RATIO FOR ELECTRON                    00593400
C        RINF      RYDBERG CONSTANT                                     00593500
C        GAMMA     GYROMAGNETIC RATIO OF PROTON (CORRECTED FOR H2O)     00593600
C        MUB       BOHR MAGNETON                                        00593700
C        R         GAS CONSTANT                                         00593800
C        K         BOLTZMANN CONSTANT                                   00593900
C        CONE      FIRST RADIATION CONSTANT                             00594000
C        CTWO      SECOND RADIATION CONSTANT                            00594100
C        SIGMA     STEPHAN-BOLTZMANN CONSTANT                           00594200
C        G         GRAVITATIONAL CONSTANT                               00594300
C                                                                       00594400
C                                                                       00594500
C     IF NAME .LE. 0, NAME = INDEX FROM MKS,CGS  0 = CGS, -1 = MKS      00594600
C                                                                       00594700
      IF(NAME.GT.0)GO TO 10                                             00594800
      J=NAME                                                            00594900
      RETURN                                                            00595000
   10 DO 20 IM=1,20                                                     00595100
      I = IM                                                            00595200
      IF(NAME.EQ.N(I))GO TO 30                                          00595300
  20  CONTINUE                                                          00595400
      ARG=0.                                                            00595500
      RETURN                                                            00595600
  30  I=I+I+J                                                           00595700
      ARG=P(I)                                                          00595800
      RETURN                                                            00595900
      END                                                               00596000
C  76 348      SUBROUTINE PLOT                     2 19 68              00596100
      SUBROUTINE PLOT                                                   00596200
C**** WRITTEN BY  S. PEAVY   11/ 8/67                                   00596300
C**** WRITTEN BY  S. PEAVY   10/26/67                                   00596400
C**** THIS ROUTINE PLOTS MAX. OF 5 CURVES. IF MORE THEN ONE POINT FALLS 00596500
C**** ON THE SAME POSITION A TALLY IS KEPT AND THE NUMBER IS PRINTED.   00596600
C**** THE USER MAY PROVIDE THE BOUNDS ON THE X,Y COORDINATES.           00596700
C**** IF BOUNDS ARE PROVIDED,THEY MUST APPEAR IN PAIRS AS READ NOS. IF A00596800
C**** PAIR OF REAL NOS ARE EQUAL THE PROGRAM ASSUMES THERE ARE NO BOUNDS00596900
C**** COMMANDS FOR USING THIS PLOT ARE AS FOLLOWS                       00597000
C**** FOR THE AXIS THAT PAIR REPRESENTS AND THE BOUNDS WILL BE CALCULAT-00597100
C**** ED.                                                               00597200
C**** COMMANDS FOR USING PLOT ARE AS FOLLOWS                            00597300
C**** I     PLOT Y +++,+++,... X +++                                    00597400
C**** II    PLOT Y +++,+++,....,(YMIN,YMAX) X +++ (XMIN,XMAX)           00597500
C**** III   PLOT Y +++,+++,....,(YMIN,YMAX) X ++1                       00597600
C**** IV    PLOT Y +++,+++,.... V +++ (XMIN,XMAX)                       00597700
C**** V     PLOT Y +++,+++,.... X (XMIN,XMAX) (YMIN,YMAX)               00597800
C****                                                                   00597900
C**** ERRORS                                                            00598000
C**** I  WHEN TYPE II COMMAND IS USED THERE MUST BE TWO PAIRS OF REAL   00598100
C****    NOS. OTHERWISE THE FOLLOWING MESSAGE IS PRINTED                00598200
C****       " Y BOUNDS ARE NOT SET UP CORRECTLY"                        00598300
C****  I  IF BOUNDS ARE PROVIDED, THEN THERE MUST BE FOUR REAL NOS.     00598400
C**** II  IF A SINGLE REAL NO. APPEARS AHEAD OF COLUMN NOS., THE FOLLOW-00598500
C****     ING MESSAGE WILL BE PRINTED AND NO PLOTTING WILL TAKE PLACE   00598600
C****       " Y BOUNDS ARE NOT SET UP CORRECTLY"                        00598700
C**** III IF A PLOT COMMAND ENDS WITH ONE REAL NO, THE FOLLOWING MESSAGE00598800
C****     WILL BE PRINTED AND PLOTTING WILL BE TERMINATED               00598900
C****       " X BOUNDS ARE NOT SET UP CORRECTLY"                        00599000
C**** IV  IF MORE THEN 5 PLOTS ARE REQUESTED PER GRAPH, NO GRAPH WILL BE00599100
C****     PRODUCED AND FOLLOWING MESSAGE WILL BE PRINTED.               00599200
C****       " MORE THEN 5 PLOTS WERE REQUISTED PER GRAPH"               00599300
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00599400
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00599500
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00599600
      COMMON / SCRAT / A(10000),NS                                      00599700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00599800
     ONROW,                                                             00599900
     1NCOL,NARGS,VWXYZ(8),NERROR                                        00600000
      DIMENSION ARGS(100)                                               00600100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00600200
      DIMENSION TIT(60),TITX(60)                                        00600300
      EQUIVALENCE(TIT,ITLE(1,6)),(TITX,ITLE(1,5))                       00600400
      DIMENSION X(1),KCCL(6),PRINT(101),XP(6),BOOL(5),IDGT(9)           00600500
      EQUIVALENCE (RC(1),X(1) )                                         00600600
      INTEGER PRINT,BLANK                                               00600700
      EQUIVALENCE (X0,XMIN),(X1,XMAX),(Y0,YMIN),(Y1,YMAX)               00600800
      DIMENSION IHD(3,6),IPR(101)                                       00600900
      INTEGER BOOL,COL1,COL2                                            00601000
      DATA BOOL(1),BOOL(2),BOOL(3),BOOL(4),BOOL(5)/1H.,1H*,1H+,1H,,1H-/,00601100
     1COL1,COL2/3HCOL ,3HUMN /,BLANK/ 1H  /                             00601200
      DATA IDGT(1),IDGT(2),IDGT(3),IDGT(4),IDGT(5),IDGT(6),IDGT(7),     00601300
     1 IDGT(8),IDGT(9)/1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1HX /            00601400
C**** INITIAL  SWITCHES                                                 00601500
      DATA IXPR/1HX/                                                    00601600
      IF( L2 .EQ. 5) KLIM=101                                           00601700
      IF( L2 .EQ. 14) KLIM=61                                           00601800
      KLIM1=KLIM-1                                                      00601900
      IF(L2 .EQ. 5) KIM=5                                               00602000
      IF(L2 .EQ. 14) KIM=3                                              00602100
      ISWT=1                                                            00602200
      ISWT1=0                                                           00602300
      XUP=1.E+35                                                        00602400
      XDOWN=-1.E+35                                                     00602500
      YUP=1.E+35                                                        00602600
      YDOWN=-1.E+35                                                     00602700
      NCN=0                                                             00602800
      IF(NARGS.EQ.2) GO TO 115                                          00602900
      IF(KIND(NARGS).EQ.0)  GO TO 110                                   00603000
      IF(KIND(NARGS)-KIND(NARGS-1).NE.0) GO TO 710                      00603100
C**** X OR Y BOUNDS ARE PROVIDED                                        00603200
      IF(KIND(NARGS-2).EQ.0)  GO TO 100                                 00603300
      IF(KIND(NARGS-3).EQ.0) GO TO 710                                  00603400
      ISWT=5                                                            00603500
      YUP=ARGS(NARGS)                                                   00603600
      YDOWN=ARGS(NARGS-1)                                               00603700
      XUP=ARGS(NARGS-2)                                                 00603800
      XDOWN=ARGS(NARGS-3)                                               00603900
      NARGS=NARGS-4                                                     00604000
      GO TO 115                                                         00604100
C**** X BOUNDS ARE PROVIDED                                             00604200
  100 ISWT=3                                                            00604300
      XUP=ARGS(NARGS)                                                   00604400
      XDOWN=ARGS(NARGS-1)                                               00604500
      NARGS=NARGS-2                                                     00604600
C**** CHECK TO SEE IF THERE ARE Y BOUNDS                                00604700
  110 IF(KIND(NARGS-1)-KIND(NARGS-2).NE.0) GO TO 700                    00604800
      IF(KIND(NARGS-1).EQ.0) GO TO 115                                  00604900
C**** Y LIMITS ARE PROVIDED                                             00605000
      ISWT=ISWT+1                                                       00605100
      YUP=ARGS(NARGS-1)                                                 00605200
      YDOWN=ARGS(NARGS-2)                                               00605300
      IARGS(NARGS-2)=IARGS(NARGS)                                       00605400
      KIND(NARGS-2)=0                                                   00605500
      NARGS=NARGS-2                                                     00605600
  115 DO 120 I=1,NARGS                                                  00605700
  120 KCCL(I)=IARGS(I)                                                  00605800
      M=NARGS-1                                                         00605900
      IF(NARGS.GT.6) GO TO 720                                          00606000
      CALL CHKCOL(J)                                                    00606100
      IF(J.GT.0) GO TO 730                                              00606200
C**** NO ERROR FOUND IN COLUMN NOS.                                     00606300
      IF(NERROR.GE.1) RETURN                                            00606400
C**** SEARCH FOR MAX AND MIN ON AXIS,  IF BOUNDS ARE NOT PROVIDED,      00606500
C**** OTHERWISE TALLY NO OF POINTS THAT FALL OUTSIDE OF BOUNDS .        00606600
      IF(XUP.GE.XDOWN) GO TO 122                                        00606700
      XAP=XDOWN                                                         00606800
      XAN=XUP                                                           00606900
      GO TO 124                                                         00607000
  122 XAP=XUP                                                           00607100
      XAN=XDOWN                                                         00607200
  124 IF(YUP.GE.YDOWN) GO TO 125                                        00607300
      YAP=YDOWN                                                         00607400
      YAN=YUP                                                           00607500
      GO TO 126                                                         00607600
  125 YAP=YUP                                                           00607700
      YAN=YDOWN                                                         00607800
  126 K1=IARGS(NARGS)                                                   00607900
      K2=K1-1+NRMAX                                                     00608000
      IF(ISWT-2)127,1000,135                                            00608100
  127 X1=X(K1)                                                          00608200
      X0=X1                                                             00608300
      DO  130 I=K1,K2                                                   00608400
      IF(X1.GE.X(I)) GO TO 128                                          00608500
      X1=X(I)                                                           00608600
      GO TO 130                                                         00608700
  128 IF(X0.LE.X(I)) GO TO 130                                          00608800
      X0=X(I)                                                           00608900
  130 CONTINUE                                                          00609000
  133 XAP=X1                                                            00609100
      XAN=X0                                                            00609200
  135 GO TO(138,170 ,136,170, 170 ),ISWT                                00609300
  136 KEY=2                                                             00609400
      GO TO 140                                                         00609500
  138 KEY=1                                                             00609600
  140 DO  167 J=1,M                                                     00609700
      K1=IARGS(NARGS)                                                   00609800
      K3=IARGS(J)                                                       00609900
      K4=K3-1+NRMAX                                                     00610000
      IF(J.GT.1) GO TO 145                                              00610100
      Y1=X(K3)                                                          00610200
      Y0=Y1                                                             00610300
      KY=1                                                              00610400
  145 GO TO( 147,155  ),KEY                                             00610500
  147 DO 150  I=K3 ,K4                                                  00610600
      IF(Y1.LT.X(I)) Y1=X(I)                                            00610700
      IF(Y0.GT.X(I)) Y0=X(I)                                            00610800
  150 CONTINUE                                                          00610900
      GO TO  167                                                        00611000
  155 DO 165  I=K3,K4                                                   00611100
      IF(X(K1).GE.XAN.AND.X(K1).LE.XAP) GO TO (160,162),KY              00611200
      GO  TO 165                                                        00611300
  160 Y1=X(I)                                                           00611400
      Y0=X(I)                                                           00611500
      KY=2                                                              00611600
      GO TO 165                                                         00611700
  162 IF(Y1.LT.X(I))  Y1=X(I)                                           00611800
      IF(Y0.GT.X(I))  Y0=X(I)                                           00611900
  165 K1=K1+1                                                           00612000
  167 CONTINUE                                                          00612100
      YAP=Y1                                                            00612200
      YAN=Y0                                                            00612300
      IF(ISWT.EQ.1) GO TO 1990                                          00612400
      GO TO 180                                                         00612500
  170 Y1=YUP                                                            00612600
      Y0=YDOWN                                                          00612700
      ISWT1=1                                                           00612800
      IF(ISWT.EQ.2) GO TO 1100                                          00612900
  180 X1=XUP                                                            00613000
      X0=XDOWN                                                          00613100
      GO TO 1100                                                        00613200
C**** DETERMINE X AND Y INCREMENTS FOR PLOT                             00613300
  195 YDELTA=(YMAX-YMIN)/50.                                            00613400
      K1=IARGS(NARGS)                                                   00613500
      IF( L2 .EQ. 5) XDELTA=(XMAX-XMIN)/100.                            00613600
      IF( L2 .EQ. 14) XDELTA=(XMAX-XMIN)/60.                            00613700
      YL  =YMAX-YDELTA/2.                                               00613800
      YT=YMAX                                                           00613900
      IF(ISWT.GT.1) WRITE(IPRINT,610) NTOT,NCN                          00614000
      GO TO 2300                                                        00614100
  198 KYTL=1                                                            00614200
      IF(YMAX.LT.YMIN)  KYTL=2                                          00614300
      KXTL=1                                                            00614400
      IF(XMAX.LT.XMIN)  KXTL=2                                          00614500
      ITB=1                                                             00614600
C**** THE I LOOP CONTROLS THE 5 DIVISIONS OF THE Y ORDINATE             00614700
      DO 350  I=1,6                                                     00614800
      L=1                                                               00614900
C**** THE J LOOP IS FOR EACH LINE OF PRINT WITHIN THE DIVISIONS         00615000
      DO 350  J=1,10                                                    00615100
C**** BLANK OUT PRINT BUFFER LINE.                                      00615200
      DO 200 K=1,KLIM                                                   00615300
  200 PRINT(K)=BLANK                                                    00615400
C**** THE KK INDEX IS FOR EACH CURVE.  KK LESS THAN 6.                  00615500
      DO 270 KK=1,M                                                     00615600
      K3=IARGS(KK)                                                      00615700
      K4=K3-1+NRMAX                                                     00615800
      K5=K1                                                             00615900
C**** THIS DETERMINES IF Y(K) VALUE IS ON THE PRESENT PRINT LINE        00616000
      DO 260 K=K3,K4                                                    00616100
      GO TO (202,201), KYTL                                             00616200
  202 IF(X(K)-YT )205,205,260                                           00616300
  205 IF(X(K)-YL )260,260,210                                           00616400
  201 IF(X(K)-YL) 203,203,260                                           00616500
  203 IF(X(K)-YT) 260,260,210                                           00616600
C**** YES. Y(K) BELONGS ON THIS PRINT LINE                              00616700
C**** THEREFORE DETERMIND  WHERE ALL THE X(K5) FALL ON THE X-AXIS       00616800
  210 XL=XMIN                                                           00616900
      XT=XMIN+XDELTA/2.                                                 00617000
      DO 255 KA=1,KLIM                                                  00617100
      GO TO (212,211) , KXTL                                            00617200
  211 IF(X(K5)-XT) 250,250,213                                          00617300
  213 IF(X(K5) -XL)  220,220,250                                        00617400
  212 IF(X(K5)-XL ) 250,215,215                                         00617500
  215 IF(X(K5)-XT ) 220,250,250                                         00617600
  220 IF(PRINT(KA)-BLANK)240,230,240                                    00617700
  230 PRINT(KA)=BOOL(KK)                                                00617800
      GO TO 260                                                         00617900
C**** IF MORE THEN ONE POINT FALLS ON THE PRINT POSITION, TALLY THE NO. 00618000
C**** OF POINTS.                                                        00618100
  240 DO 242 KKK=1,9                                                    00618200
      IF(PRINT(KA)-IDGT(KKK)) 242,244,242                               00618300
  242 CONTINUE                                                          00618400
      PRINT(KA)=IDGT(1)                                                 00618500
      GO TO 260                                                         00618600
  244 IF(PRINT(KA).NE.IDGT(9)) PRINT(KA)=IDGT(KKK+1)                    00618700
      GO TO 260                                                         00618800
  250 XL=XT                                                             00618900
  255 XT=XT+XDELTA                                                      00619000
  260 K5=K5+1                                                           00619100
  270 CONTINUE                                                          00619200
      YP=YT*YL                                                          00619300
      YT=YL                                                             00619400
      YL=YL-YDELTA                                                      00619500
      GO TO (280,300),L                                                 00619600
  280 IF(I-5) 285,285,400                                               00619700
  285 L=2                                                               00619800
      YS = YT + YDELTA / 2.                                             00619900
C**** THIS PATH IS EXECUTED ONCE IN EVERY DIVISION OF THE Y-AXIS. EVERY 00620000
C**** TENTH LINE, STARTING WITH ZERO LINE                               00620100
      IF(YP   ) 286,286,295                                             00620200
  286 IF(L2 .EQ. 5)WRITE(IPRINT,299) TIT(ITB),YS, PRINT                 00620300
      IF(L2 .EQ.  14) WRITE(IPRINT,298) TIT(ITB),YS,(PRINT(IB),IB=1,61) 00620400
      GO TO 350                                                         00620500
  290 FORMAT(1X,A1,1PE11.4,1H+,101A1,1H+)                               00620600
  291 FORMAT(1X,A1,1PE11.4,1H+,61A1,1H+)                                00620700
  295 IF(L2 .EQ. 5) WRITE(IPRINT,290) TIT(ITB),YS, PRINT                00620800
      IF(L2 .EQ.14) WRITE(IPRINT,291) TIT(ITB),YS,(PRINT(IB),IB=1,61)   00620900
      GO TO 350                                                         00621000
  298 FORMAT(1X,A1,1PE11.4,1HX,61A1,1HX)                                00621100
  299 FORMAT(1X,A1,1PE11.4,1HX,101A1,1HX)                               00621200
  300 IF(YP   ) 302,302,306                                             00621300
C**** PRINTS LINE                                                       00621400
  302 IF( L2 .EQ. 5) WRITE(IPRINT,304) TIT(ITB), PRINT                  00621500
      IF( L2 .EQ. 14) WRITE(IPRINT,303) TIT(ITB),(PRINT(IB),IB=1,61)    00621600
  303 FORMAT(1X,A1,11X,1HX,61A1,1HX)                                    00621700
  304 FORMAT(1X,A1,11X,1HX,101A1,1HX)                                   00621800
      GO TO 350                                                         00621900
  306 IF( L2 .EQ. 5) WRITE(IPRINT,310) TIT(ITB), PRINT                  00622000
      IF( L2 .EQ. 14) WRITE(IPRINT,309) TIT(ITB),(PRINT(IB),IB=1,61)    00622100
  309 FORMAT(1X,A1,11X,1H-,61A1,1H-)                                    00622200
  310 FORMAT(1X,A1,11X,1H-,101A1,1H-)                                   00622300
  350 ITB=ITB+1                                                         00622400
  400 IF(YP      ) 402,402,406                                          00622500
  402 IF( L2 .EQ. 5) WRITE(IPRINT,299) TIT(51),YMIN,PRINT               00622600
      IF( L2 .EQ. 14) WRITE(IPRINT,298) TIT(51),YMIN,(PRINT(IB),IB=1,61)00622700
      GO  TO 408                                                        00622800
  406 IF( L2 .EQ. 5) WRITE(IPRINT,290) TIT(51),YMIN,PRINT               00622900
      IF( L2 .EQ. 14) WRITE(IPRINT,291) TIT(51),YMIN,(PRINT(IB),IB=1,61)00623000
C**** LAST LINE OF PRINT OUT PLUS X VALUES ALONG X-AXIS.                00623100
  408 IF( L2 .EQ. 14) GO TO 430                                         00623200
      WRITE(IPRINT,620)IPR                                              00623300
      WRITE(IPRINT,420) XP                                              00623400
  409 WRITE(IPRINT,630) TITX                                            00623500
  420 FORMAT(6(7X,1PE13.4))                                             00623600
      RETURN                                                            00623700
  430 WRITE(IPRINT,620) (IPR(I),I=1,61)                                 00623800
      WRITE(IPRINT,420) (XP(I),I=1,4)                                   00623900
      GO TO 409                                                         00624000
  600 FORMAT(12H ABS- COLUMN,I6, 7H; ORD- ,5(2A3,I5,2H (,A1,4H),  ))    00624100
  605 FORMAT( 6H ABS- ,2A6,6H;ORD- ,5(2A6,2H (,A1,3H), ))               00624200
  610 FORMAT(29H TOTAL NO. OF PTS. PLOTTED IS,I5,60H AND NO. NOT PLOTTED00624300
     1  BECAUSE THEY FALL OUTSIDE OF BOUNDS IS,I5  )                    00624400
  620 FORMAT(14X,101A1)                                                 00624500
  630 FORMAT(34X,60A1)                                                  00624600
C**** THIS PRINTS OUT THAT "Y BOUNDS ARE NOT SET UP CORRECTLY".         00624700
 700  CONTINUE                                                          00624800
C**** THIS PRINTS OUT THAT "X BOUNDS ARE NOT SET UP CORRECTLY".         00624900
 710  CONTINUE                                                          00625000
C**** THIS PRINT "COL. NOS. APPEAR AS ARGUMENTS".                       00625100
 730  CALL ERROR(20)                                                    00625200
 725  NERROR = NERROR - 1                                               00625300
      RETURN                                                            00625400
C**** THIS PRINTS THAT "MORE THEN 5 PLOTS WERE REQUISTED PER GRAPH".    00625500
 720  CALL ERROR(10)                                                    00625600
      GO TO 725                                                         00625700
 1000 KEY=1                                                             00625800
 1010 DO  1050 IK=1,M                                                   00625900
      IKK=IARGS(IK)                                                     00626000
      DO  1045 I=K1,K2                                                  00626100
      IF(X(IKK).GE.YAN.AND.X(IKK).LE.YAP) GO TO (1030,1040),KEY         00626200
      GO  TO 1045                                                       00626300
 1030 X1=X(I)                                                           00626400
      X0=X1                                                             00626500
      KEY=2                                                             00626600
      GO TO 1045                                                        00626700
 1040 IF(X1.LT.X(I)) X1=X(I)                                            00626800
      IF(X0.GT.X(I)) X0=X(I)                                            00626900
 1045 IKK=IKK+1                                                         00627000
 1050 CONTINUE                                                          00627100
      IF(KEY.EQ.2)  GO TO 133                                           00627200
      X0=XDOWN                                                          00627300
      X1=XUP                                                            00627400
      GO TO 133                                                         00627500
 1100 DO 1120  J=1,M                                                    00627600
      K1=IARGS(NARGS)                                                   00627700
      K3=IARGS(J)                                                       00627800
      K4=K3-1+NRMAX                                                     00627900
      DO 1120  I=K3,K4                                                  00628000
      IF(X(I).GT.YAP.OR.X(I).LT.YAN) GO TO 1110                         00628100
      IF(X(K1).LE.XAP.AND.X(K1).GE.XAN) GO TO 1120                      00628200
 1110 NCN=NCN+1                                                         00628300
 1120 K1=K1+1                                                           00628400
      NTOT=M*NRMAX-NCN                                                  00628500
C**** DETERMINE TYPE OF HEADINGS TO BE PRINTED                          00628600
 1990 KTLE=0                                                            00628700
      DO 2000  I=1,NARGS                                                00628800
      J=KCCL(I)                                                         00628900
      IF(J.GT.50.OR.IHEAD(1,J).EQ.0) GO TO 2005                         00629000
      IHD(1,I)=IHEAD(1,J)                                               00629100
      IHD(2,I)=IHEAD(2,J)                                               00629200
      IHD(3,I) = IHEAD(3,J)                                             00629300
      KTLE=KTLE+1                                                       00629400
 2000 CONTINUE                                                          00629500
C**** STARTS A NEW PAGE AND PRINTS TITLE 1 AND 2                        00629600
 2005 CALL PAGE(2)                                                      00629700
      IF(KTLE.EQ.NARGS)  GO TO 2010                                     00629800
      WRITE(IPRINT,600) KCCL(NARGS),(COL1,COL2,KCCL(I),BOOL(I),I=1,M)   00629900
      GO TO 195                                                         00630000
 2010 WRITE (IPRINT,605)  (IHD(I,NARGS),I=1,2),  ((IHD(I,J),I=1,2),     00630100
     1 BOOL(J  ),J=1,M)                                                 00630200
      GO TO 195                                                         00630300
 2300 XP(1)=XMIN                                                        00630400
      IF( L2 .EQ. 5) XP(6)=XMAX                                         00630500
      IF( L2 .EQ. 14) XP(4)=XMAX                                        00630600
      XR=20.*XDELTA                                                     00630700
      DO 2310 I=2,KIM                                                   00630800
 2310 XP(I)=XP(I-1)+XR                                                  00630900
      DO 2320 J=1,KLIM1                                                 00631000
 2320 IPR(J)=BOOL(5)                                                    00631100
      DO 2330 I=1,KLIM,10                                               00631200
 2330 IPR(I)=BOOL(3)                                                    00631300
      IF(XMIN*XMAX.GE.0) GO  TO 2370                                    00631400
      J=0                                                               00631500
      DO 2340 I=2,KIM                                                   00631600
      IF(XP(I-1)*XP(I))2345,2360,2340                                   00631700
 2340 CONTINUE                                                          00631800
      I=KIM                                                             00631900
      GO TO 2360                                                        00632000
 2345  XXP=XP(I-1)+XDELTA                                               00632100
      DO 2350   J=1,20                                                  00632200
      IF(XP(I-1)*XXP            .LE.0.0) GO TO 2360                     00632300
 2350 XXP=XXP+XDELTA                                                    00632400
      J=20                                                              00632500
 2360 N=(I-2)*20+J                                                      00632600
      IPR(N)=IXPR                                                       00632700
      IPR(N)=IXPR                                                       00632800
 2370 IF( L2 .EQ. 5) WRITE(IPRINT,620) IPR                              00632900
      IF(L2 .EQ. 14) WRITE(IPRINT,620) (IPR(I),I=1,61)                  00633000
      GO TO 198                                                         00633100
      END                                                               00633200
C  77  13      SUBROUTINE PNT(ND15)                2 19 68              00633300
      SUBROUTINE PNT(ND15)                                              00633400
      COMMON /BLOCKD/R(10100),IA(100),KI(100),ART(100),NR,NRO,NC,NARGS, 00633500
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00633600
      DIMENSION ARGS(100)                                               00633700
      EQUIVALENCE( ARGS(1), R(10001) )                                  00633800
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00633900
      COMMON / SCRAT / SCRA(10000),NS                                   00634000
                                                                        00634100
                                                                        00634200
    1 PRINT 40, (SCRA(I), I = 1,ND15)                                   00634300
      RETURN                                                            00634400
   40 FORMAT(8E16.8)                                                    00634500
      END                                                               00634600
C  78  71      SUBROUTINE PRINTX                   2 19 68              00634700
      SUBROUTINE PRINTX                                                 00634800
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00634900
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00635000
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00635100
      DIMENSION ARGS(100)                                               00635200
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00635300
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00635400
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00635500
      DIMENSION KFMT( 17 )                                              00635600
      IF ( L1 .EQ. 2 .OR. L1 .EQ. 3 ) IF( NARGS ) 20,20,45              00635700
      IF( NARGS .GT. 1 ) GO TO 40                                       00635800
C                                                                       00635900
C     L1 = 2 FOR PRINT, L1 = 6 FOR ABRIDGE                              00636000
C     L1 = 3 FOR PUNCH                                                  00636100
C                                                                       00636200
  20  CALL ERROR( 205 )                                                 00636300
  30  RETURN                                                            00636400
  40  LL = IARGS( 1 )                                                   00636500
      IARGS( 1 ) = 1                                                    00636600
      IF( LL .LE. 0 .OR. LL .GT. NROW ) GO TO 20                        00636700
  45  DO 46 I = 1, NARGS                                                00636800
  46  IARGS( I+50 ) = IARGS( I )                                        00636900
  50  CALL CHKCOL( I )                                                  00637000
      IF( I .NE. 0 ) GO TO 20                                           00637100
  60  IF( NERROR .NE. 0 ) GO TO 30                                      00637200
      CALL SETFMT( KFMT )                                               00637300
      IF( L1 .EQ. 2 ) GO TO 80                                          00637400
      IF ( L1 .EQ. 3 ) GO TO 150                                        00637500
C                                                                       00637600
C     ABRIDGE                                                           00637700
C                                                                       00637800
      DO 70 I = 2, NARGS                                                00637900
      J = IARGS( I ) + LL                                               00638000
  70  ARGS( I ) = RC( J-1 )                                             00638100
      WRITE( IPRINT, KFMT ) ( ARGS( I ), I = 2, NARGS )                 00638200
      GO TO 30                                                          00638300
C                                                                       00638400
C     PRINT                                                             00638500
C                                                                       00638600
  80  LL = NRMAX                                                        00638700
  90  IF( LL .GT. 51 ) GO TO 100                                        00638800
      J = LL                                                            00638900
      LL = 0                                                            00639000
      GO TO 110                                                         00639100
 100  LL = LL - 50                                                      00639200
      J = 50                                                            00639300
 110  CALL PAGE( 4 )                                                    00639400
      IF( L2 .EQ. 1 ) CALL HEADS                                        00639500
      WRITE( IPRINT, 130 )                                              00639600
      DO 140 M = 1, J                                                   00639700
      DO 120 I = 1, NARGS                                               00639800
      K = IARGS( I )                                                    00639900
      IARGS( I ) = IARGS( I ) + 1                                       00640000
 120  ARGS( I ) = RC( K )                                               00640100
      WRITE( IPRINT, KFMT ) ( ARGS( I ), I = 1, NARGS )                 00640200
      IF( MOD( M, 10 ) .EQ. 0 ) WRITE( IPRINT, 130 )                    00640300
 130  FORMAT(1X)                                                        00640400
 140  CONTINUE                                                          00640500
      IF( LL ) 30, 30, 90                                               00640600
C                                                                       00640700
C     PUNCH                                                             00640800
C                                                                       00640900
  150 DO 170 M=1,NRMAX                                                  00641000
      DO 160 I=1,NARGS                                                  00641100
      K = IARGS( I )                                                    00641200
      IARGS( I ) = IARGS( I ) + 1                                       00641300
  160 ARGS( I ) = RC( K )                                               00641400
      WRITE(IPUNCH, KFMT ) ( ARGS( I ) , I=1,NARGS )                    00641500
  170 CONTINUE                                                          00641600
      RETURN                                                            00641700
      END                                                               00641800
C  79  89      SUBROUTINE PROB (VNU1,VNU2,F,Q)     2 19 68              00641900
      SUBROUTINE PROB (VNU1,VNU2,F,Q)                                   00642000
      DOUBLE PRECISION C,A,X,W,ONE,B,TA,TB,G                            00642100
      DATA C/.6366197723675814D0/,EP/1.E-5/,ONE/1.D0/,TWO/2.0/,ONEP/1./,00642200
     1 P5/.5/                                                           00642300
      NU1=VNU1+EP                                                       00642400
      NU2=VNU2+EP                                                       00642500
      V1=NU1                                                            00642600
      V2=NU2                                                            00642700
      IF (ABS(V1-VNU1).GT.EP) GO TO 310                                 00642800
      IF( ABS(V2-VNU2).GT.EP) GO TO 310                                 00642900
      IF(F.GE.0.) GO TO 80                                              00643000
      F=0.                                                              00643100
C**** ERROR " SET F=0  SINCE  F LESS THEN 0"                            00643200
      CALL ERROR (206)                                                  00643300
   80 IF(NU1.LT.0.OR.NU2.LT.0) GO TO 300                                00643400
  105 MNU1=MOD(NU1,2)                                                   00643500
      MNU2=MOD(NU2,2)                                                   00643600
      IF(MNU2.NE.0) GO TO 120                                           00643700
      I1=NU2/2-1                                                        00643800
      X=V2/(V2+V1*F)                                                    00643900
      V4=V1/TWO                                                         00644000
      I4=NU1                                                            00644100
   90 A=ONE                                                             00644200
      IF(I1.EQ.0) GO TO 110                                             00644300
      W=A                                                               00644400
      DO  100 I=1,I1                                                    00644500
      T=I                                                               00644600
      W=((V4+T-ONEP)/T)*X*W                                             00644700
  100 A=A+W                                                             00644800
  110 Q=A*(ONE-X)**V4                                                   00644900
      IF(  I4.EQ.NU1) Q=ONEP-Q                                          00645000
  115 IF(Q.LT.0.) Q=0                                                   00645100
      IF(Q.GT.ONEP)  Q=ONEP                                             00645200
      RETURN                                                            00645300
  120 IF(MNU1.NE.0) GO TO 130                                           00645400
      I1=NU1/2-1                                                        00645500
      X=ONEP-V2/(V2+V1*F)                                               00645600
      V4=V2/TWO                                                         00645700
      I4=NU2                                                            00645800
      GO TO 90                                                          00645900
  130 IF(NU2.NE.1) GO TO 170                                            00646000
      IF(NU1.NE.1) GO TO 140                                            00646100
      Q=C*ATAN(ONEP/FSQRT(F))                                           00646200
      GO TO 115                                                         00646300
  140 X=ATAN(FSQRT(V2/(V1*F)))                                          00646400
      I1= (NU1-3)/2                                                     00646500
      IS=1                                                              00646600
  145 TB=DSIN(X)                                                        00646700
      A=DCOS(X)                                                         00646800
      IF(I1.EQ.0) GO TO 160                                             00646900
      TA=A**2                                                           00647000
      W=A                                                               00647100
      DO  150 I=1,I1                                                    00647200
      V3=I                                                              00647300
      W= V3/(V3+P5)*TA*W                                                00647400
  150 A=A+W                                                             00647500
  160 A=C*(X+TB*A)                                                      00647600
      Q=A                                                               00647700
      GO TO (115,180),IS                                                00647800
  170 X=ATAN(FSQRT(V1*F/V2))                                            00647900
      I1= (NU2-3)/2                                                     00648000
      IS=2                                                              00648100
      GO TO 145                                                         00648200
  180 IF(NU1.NE.1) GO TO 190                                            00648300
      Q=ONE-A                                                           00648400
      GO TO 115                                                         00648500
  190 I1=(NU1-3)/2                                                      00648600
      B=ONE                                                             00648700
      IF(I1.EQ.0) GO TO 210                                             00648800
      W=B                                                               00648900
      DO  200  I=1,I1                                                   00649000
      V3=I                                                              00649100
      W= (V2+TWO*V3-ONEP)/(TWO*V3+ONEP)*TB**2*W                         00649200
  200 B=B+W                                                             00649300
  210 G=C                                                               00649400
      I1=(NU2-1)/2                                                      00649500
      DO 220 I=1,I1                                                     00649600
      V3=I                                                              00649700
  220 G=(TWO*V3)/(TWO*V3-ONEP)*G                                        00649800
      Q=ONE-A+G*TB*DCOS(X)**NU2*B                                       00649900
      GO TO 115                                                         00650000
C**** PRINT " EITHER NU1 OR NU2 IS LESS THEN 1 "                        00650100
  300 CALL ERROR (207)                                                  00650200
      RETURN                                                            00650300
C**** PRINT " EITHER NU1 OR NU2 IS NOT A INTEGER  PROGRAM USES LARGEST  00650400
C**** INTEGER CONTAINED "                                               00650500
  310 CALL ERROR(208)                                                   00650600
      GO TO 105                                                         00650700
      END                                                               00650800
C  80  55      SUBROUTINE PROROW                   2 19 68              00650900
      SUBROUTINE PROROW                                                 00651000
C     PROGRAMMED BY CARLA MESSINA  MAY,1967                             00651100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00651200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00651300
      DIMENSION ARGS(100)                                               00651400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00651500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00651600
      COMMON / SCRAT / A(10000),NS                                      00651700
      IF (NARGS - 3)  10,40,40                                          00651800
10    K = 10                                                            00651900
20    CALL ERROR(K)                                                     00652000
30    RETURN                                                            00652100
40    CALL CHKCOL(J)                                                    00652200
      IF (J) 50,60,50                                                   00652300
50    K = 3                                                             00652400
      GO TO 20                                                          00652500
60    IF (NRMAX) 70,70,80                                               00652600
70    K = 9                                                             00652700
      GO TO 20                                                          00652800
80    IF (NERROR .NE. 0) RETURN                                         00652900
      DO  100  I=1,NRMAX                                                00653000
      A(I) = 0.0                                                        00653100
      GO TO (100,90), L2                                                00653200
90    A(I) = 1.0                                                        00653300
100   CONTINUE                                                          00653400
      IF( NARGS - 4 ) 110, 200, 200                                     00653500
110   IF (IARGS(1) - IARGS(2)) 120,120,50                               00653600
120   K = IARGS(1)                                                      00653700
      DO  150  I=1,NRMAX                                                00653800
      J = K + I - 1                                                     00653900
      GO TO (130,140), L2                                               00654000
130   A(I) = A(I) + RC(J)                                               00654100
      GO TO 150                                                         00654200
140   A(I) = A(I)*RC(J)                                                 00654300
150   CONTINUE                                                          00654400
      IF (IARGS(1) + NROW - IARGS(2))  160,160,170                      00654500
160   IARGS(1) = IARGS(1) + NROW                                        00654600
      GO TO 120                                                         00654700
170   K = IARGS(NARGS)                                                  00654800
      DO 180  I=1,NRMAX                                                 00654900
      J = K + I - 1                                                     00655000
180   RC(J) = A(I)                                                      00655100
      GO TO 30                                                          00655200
200   II = NARGS - 1                                                    00655300
      DO  250  L=1,II                                                   00655400
      K = IARGS(L)                                                      00655500
      DO  250  I=1,NRMAX                                                00655600
      J = K + I - 1                                                     00655700
      GO TO (230,240), L2                                               00655800
230   A(I) = A(I)+RC(J)                                                 00655900
      GO TO 250                                                         00656000
240   A(I) = A(I)*RC(J)                                                 00656100
250   CONTINUE                                                          00656200
      GO TO 170                                                         00656300
      END                                                               00656400
C  81  19      FUNCTION QNORML(X)                  2 19 68              00656500
      FUNCTION QNORML(X)                                                00656600
C                                                                       00656700
C     A FUNCTION FROM AMS 55 CHAPTER 26 TO COMPUTE THE INVERSE NORMAL   00656800
C     INTEGRAL FOR THE PROBABILITY  X                                   00656900
C                                                                       00657000
      IF ( X .LE. 0.0 .OR. X .GE. 1.0 ) GO TO 10                        00657100
      IF (X .EQ. 0.5) GO TO 1                                           00657200
      GO TO 2                                                           00657300
   10 CALL AERR( 3 )                                                    00657400
    1 QNORML=0.0                                                        00657500
      RETURN                                                            00657600
    2 P=X                                                               00657700
      IF (X .GT. 0.5) P=1.0-X                                           00657800
      T=SQRT(ALOG(1.0/(P**2)) )                                         00657900
      QNORML=T-(2.515517+.802853*T+.010328*T**2)/(1.0+1.432788*T+.18926900658000
     1*T**2+.001308*T**3)                                               00658100
      IF ( X .LT. 0.5 ) QNORML = -1.0 * QNORML                          00658200
      RETURN                                                            00658300
      END                                                               00658400
C  82  25      SUBROUTINE READQ                    2 19 68              00658500
      SUBROUTINE READQ                                                  00658600
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00658700
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00658800
      DIMENSION ARGS(100)                                               00658900
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00659000
      COMMON / QRS / NDROW, IFLAG, J, NNARG                             00659100
      IF( IFLAG .NE. 0 ) GO TO 99                                       00659200
      IF( J .LT. NROW ) GO TO 10                                        00659300
      IFLAG = 1                                                         00659400
      CALL ERROR( 201 )                                                 00659500
      GO TO 99                                                          00659600
C     NNARG CONTAINS NARGS OF READ COMMAND                              00659700
C     IARGS(51) THRU IARGS(NNARG+50) CONTAINS ADDRESSES OF COLUMN TOPS  00659800
  10  DO 30 I = 1, NNARG                                                00659900
      K = IARGS( I + 50 ) + J                                           00660000
      IF( KIND( I ) .EQ. 0 ) GO TO 20                                   00660100
      RC( K ) = ARGS( I )                                               00660200
      GO TO 30                                                          00660300
  20  RC( K ) = IARGS( I )                                              00660400
  30  CONTINUE                                                          00660500
C     J IS CARD COUNT. IT COUNTS FROM ZERO.                             00660600
      J = J + 1                                                         00660700
      NRMAX = J                                                         00660800
  99  RETURN                                                            00660900
      END                                                               00661000
C  83  77      SUBROUTINE READX                    2 19 68              00661100
      SUBROUTINE READX                                                  00661200
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00661300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00661400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00661500
      DIMENSION ARGS(100)                                               00661600
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00661700
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00661800
      COMMON / QRS / NDROW, IFLAG, J, NNARG                             00661900
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00662000
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00662100
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00662200
      DIMENSION KFMT(17)                                                00662300
      IF( L2 .NE. 1 ) GO TO 200                                         00662400
      ISRFLG = 0                                                        00662500
      IF( NARGS .GT. 0 ) GO TO 10                                       00662600
   5  CALL ERROR( 10 )                                                  00662700
      GO TO 99                                                          00662800
  10  MODE = 2                                                          00662900
      CALL CHKCOL( I )                                                  00663000
      IF( I .EQ. 0 ) GO TO 20                                           00663100
  15  CALL ERROR( 3 )                                                   00663200
      GO TO 99                                                          00663300
  20  IF( NERROR .NE. 0 ) GO TO 99                                      00663400
      DO 30 I = 1, NARGS                                                00663500
      IARGS( I + 50 ) = IARGS( I )                                      00663600
      IARGS( I ) = 0                                                    00663700
  30  ARGS( I ) = 0.                                                    00663800
      IFLAG = 0                                                         00663900
      J = 0                                                             00664000
      NNARG = NARGS                                                     00664100
      GO TO 100                                                         00664200
  99  IFLAG = 1                                                         00664300
 100  RETURN                                                            00664400
C                                                                       00664500
C                  FORMATTED READ                                       00664600
C                  READ X  N  C C C C                                   00664700
C                                                                       00664800
C                  N = NUMBER OF CARDS TO READ. IF N = 0, READ UNTIL A  00664900
C                  BLANK CARD IS FOUND                                  00665000
C                  X IS THE FORMAT IDENTIFIER, A,B,C,D,E,F              00665100
C                                                                       00665200
 200  IF( NARGS .LE. 1 ) GO TO 5                                        00665300
C                  SETUP FORMAT                                         00665400
      CALL SETFMT( KFMT )                                               00665500
C                  CHECK AND CONVERT ARGUMENTS                          00665600
      DO 210 I = 2, NARGS                                               00665700
      CALL ADRESS( I, IARGS( I ) )                                      00665800
 210  IF( IARGS( I ) .LE. 0 ) GO TO 311                                 00665900
      IF( IARGS( 1 ) ) 15, 220, 230                                     00666000
 220  N = 10000                                                         00666100
      GO TO 240                                                         00666200
 230  N = IARGS( 1 )                                                    00666300
 240  DO 280 I = 1, N                                                   00666400
      READ( INUNIT, KFMT ) ( ARGS( J ), J = 2, NARGS )                  00666500
C                  CHECK IF LOOKING FOR BLANK CARD                      00666600
      IF( IARGS( 1 ) .NE. 0 ) GO TO 260                                 00666700
      DO 250 J = 2, NARGS                                               00666800
 250  IF( ARGS( J ) .NE. 0. ) GO TO 260                                 00666900
C                  BLANK CARD FOUND, TERMINATE READ.                    00667000
      GO TO 290                                                         00667100
C                  IF THERE IS TOO MUCH DATA, DO NOT ENTER EXCESS       00667200
 260  IF( I .GT. NROW ) GO TO 280                                       00667300
      DO 270 J = 2, NARGS                                               00667400
      K = IARGS( J )                                                    00667500
      IARGS( J ) = K + 1                                                00667600
 270  RC( K ) = ARGS( J )                                               00667700
 280  CONTINUE                                                          00667800
      I = N + 1                                                         00667900
 290  I = I - 1                                                         00668000
      NRMAX = MAX0( NRMAX, MIN0( I, NROW ) )                            00668100
      WRITE( ISCRAT, 300 ) I                                            00668200
 300  FORMAT(5X,I4,31H DATA CARDS READ BUT NOT LISTED,44X)              00668300
      IF( I .GT. NROW ) CALL ERROR( 201 )                               00668400
      GO TO 100                                                         00668500
 311  CALL ERROR( 11 )                                                  00668600
      GO TO 100                                                         00668700
      END                                                               00668800
C  84  33      SUBROUTINE RESET                    2 19 68              00668900
      SUBROUTINE RESET                                                  00669000
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00669100
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00669200
      COMMON / BLOCKF / NCTOP                                           00669300
      DIMENSION ARGS(100)                                               00669400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00669500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00669600
      IF ( NARGS .EQ. 1 ) IF ( L2 - 2 )30,110,100                       00669700
      K = 10                                                            00669800
  10  CALL ERROR( K )                                                   00669900
  20  RETURN                                                            00670000
C         RESET NRMAX                                                   00670100
  30  IF( KIND(1) .NE. 0 ) IARGS(1) = ARGS(1)                           00670200
  40  IF( IARGS( 1 ) .GE. 0 .AND. IARGS( 1 ) .LE. NROW ) GO TO 50       00670300
      K = 3                                                             00670400
      GO TO 10                                                          00670500
  50  NRMAX = IARGS( 1 )                                                00670600
      GO TO 20                                                          00670700
C         RESET V,W,X,Y,Z                                               00670800
 100  IF( KIND(1) .EQ. 0 ) ARGS(1) = IARGS(1)                           00670900
      VWXYZ( L2-2 ) = ARGS( 1 )                                         00671000
      GO TO 20                                                          00671100
C         RESET COLTOP                                                  00671200
  110 IF ( KIND( 1 ) .NE. 0 ) IARGS(1) = ARGS(1)                        00671300
      IF ( IARGS ( 1 ) .GE. 0 .AND. IARGS( 1 ) .LE. (NROW+NCTOP-1))     00671400
     1 GO TO 120                                                        00671500
      K = 3                                                             00671600
      GO TO 10                                                          00671700
  120 J = NCTOP                                                         00671800
      NCTOP = IARGS( 1 )                                                00671900
      NROW  = NROW + ( J - NCTOP )                                      00672000
      GO TO 20                                                          00672100
      END                                                               00672200
C  85  23      SUBROUTINE RNDOWN                   2 19 68              00672300
      SUBROUTINE RNDOWN                                                 00672400
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00672500
      COMMON / BLOCKX / INDEX( 6, 8 ), LEVEL                            00672600
C                                                                       00672700
C     IF AN ERROR IS MADE IN A STORED STATEMENT, THIS ROUTINE PRINTS    00672800
C     OUT EXACTLY WHEN AND WHERE IT OCCURRED.                           00672900
C                                                                       00673000
      A = FLOAT( INDEX( 6, LEVEL ) ) / 10.                              00673100
      WRITE( ISCRAT, 10 ) A                                             00673200
  10  FORMAT(31H IN COMMAND AT STATEMENT NUMBER,F6.1,47X)               00673300
      N = LEVEL - 1                                                     00673400
  20  IF( N ) 70, 50, 30                                                00673500
  30  A = FLOAT( INDEX( 6, N ) ) / 10.                                  00673600
      WRITE( ISCRAT, 40 ) INDEX( 5, N + 1 ), INDEX( 4, N + 1 ), A       00673700
  40  FORMAT(10H CYCLE NO.,I4,3H OF,I4,24H OF PERFORM AT STATEMENT,F6.1,00673800
     1 33X)                                                             00673900
      N = N - 1                                                         00674000
      GO TO 20                                                          00674100
  50  WRITE( ISCRAT, 60 ) INDEX( 5, 1 ), INDEX( 4, 1 )                  00674200
  60  FORMAT(10H CYCLE NO.,I4,3H OF,I4,31H OF EXTERNAL PERFORM STATEMENT00674300
     1.,32X)                                                            00674400
  70  RETURN                                                            00674500
      END                                                               00674600
C  86 177      SUBROUTINE SELECT                   2 19 68              00674700
      SUBROUTINE SELECT                                                 00674800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00674900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00675000
      DIMENSION ARGS(100)                                               00675100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00675200
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00675300
      COMMON / SCRAT / A(10000),NS                                      00675400
C                                                                       00675500
C ITYPE=1     SELECT IN COL ++ VALUES APPROX COL ++ TO WITHIN **,       00675600
C                  STORE IN COL ++                                      00675700
C ITYPE=1     SELECT IN COL ++ VALUES APPROX COL ++ TO WITHIN **,       00675800
C                  STORE IN COL ++ TO COL ++                            00675900
C ITYPE=1     SELECT IN COL ++ VALUES APPROX COL ++ TO WITHIN **,       00676000
C                  STORE ++ TO ++, STORE NUMBER FALLING WITHIN TOL IN CO00676100
C                                                                       00676200
C ITYPE=2     SEARCH IN COL ++ FOR NUMBERS IN ++, TRANSFER CORRESP VALUE00676300
C                  FROM ++ INTO ++, ++ INTO ++,  ETC                    00676400
C                                                                       00676500
C ITYPE=3     CENSOR COL ++ FOR $$, REPLACING BY $$, STORE IN COL ++    00676600
C                                                                       00676700
C  BY CARLA G. MESSINA  NSRDS     NBS                                   00676800
C                                                                       00676900
      GO TO (10,90,120) ,L2                                             00677000
10    IF (KIND(3)) 40,20,40                                             00677100
20    K=3                                                               00677200
30    CALL ERROR(K)                                                     00677300
35    RETURN                                                            00677400
40    IARGS(3)= IARGS(2)                                                00677500
      KIND(3)=0                                                         00677600
      IF (NARGS - 4) 50,60,70                                           00677700
50    K=10                                                              00677800
      GO TO 30                                                          00677900
60    IARGS(5)=IARGS(4)                                                 00678000
      NARGS = NARGS+ 1                                                  00678100
      KIND(5) = KIND(4)                                                 00678200
70    IF (NARGS - 6) 80,80,50                                           00678300
80    IF (IARGS(4) - IARGS(5)) 220,220,20                               00678400
90    IF (NARGS - 4) 50,100,100                                         00678500
100   IF (2*(NARGS/2) - NARGS) 50,110,50                                00678600
110   CALL CHKCOL(J)                                                    00678700
      IF (J) 20,190,20                                                  00678800
120   IF (NARGS-4) 50,130,50                                            00678900
130   DO 140 I=1,4                                                      00679000
140   CALL ADRESS(I,IARGS(I))                                           00679100
      IF (IARGS(1)) 20,20,150                                           00679200
150   IF (IARGS(4)) 20,20,160                                           00679300
160   DO 180 I=2,3                                                      00679400
      IF (KIND(I)) 180,170,180                                          00679500
170   IF (IARGS(I)) 20,20,180                                           00679600
180   CONTINUE                                                          00679700
190   IF (NERROR .NE. 0) GO TO 35                                       00679800
      IF (NRMAX) 200,200,210                                            00679900
200   K=9                                                               00680000
      GO TO 30                                                          00680100
210   GO TO (300,500,600) , L2                                          00680200
220   IF (IARGS(5) - IARGS(4) - NRMAX + 1) 110,110,230                  00680300
230   IARGS(5) =IARGS(4) + NRMAX - 1                                    00680400
      GO TO 110                                                         00680500
C   SELECT                                                              00680600
300   DO 330 I=1,NRMAX                                                  00680700
      L = IARGS( 1 ) + I - 1                                            00680800
      K = IARGS( 2 ) + I - 1                                            00680900
      J = IARGS(4) + I -1                                               00681000
      M = NRMAX + I                                                     00681100
      A(I) = RC(K)                                                      00681200
      A(M) = RC(L)                                                      00681300
310   RC(J) = 0.0                                                       00681400
      IF (J - I - IARGS(5) + 1)  320,330,330                            00681500
320   J = NROW + J                                                      00681600
      GO TO 310                                                         00681700
330   CONTINUE                                                          00681800
      ARG3= ARGS(3)                                                     00681900
      DO 480 I=1,NRMAX                                                  00682000
      I9 = 2*NRMAX + 1                                                  00682100
      I11 = 5*NRMAX                                                     00682200
      DO 335 I10 = I9, I11                                              00682300
  335 A( I10 ) = 0.0                                                    00682400
      K= NRMAX + 1                                                      00682500
      L = 2*NRMAX                                                       00682600
      M= 3*NRMAX                                                        00682700
      N = 4*NRMAX                                                       00682800
      I1 = IARGS(4) + I - 1                                             00682900
      J1 = IARGS(6) + I - 1                                             00683000
      DO 350 J=K,L                                                      00683100
      AT =  ABS(A(I) - A(J))                                            00683200
      IF ( ABS(ARG3) - AT) 350,340,340                                  00683300
340   M = M + 1                                                         00683400
      A(M) = AT                                                         00683500
      N = N + 1                                                         00683600
      A(N) = A(J)                                                       00683700
350   CONTINUE                                                          00683800
      IF (M - 3*NRMAX + 1) 360,380,400                                  00683900
360   IF (NARGS-5) 480,480,370                                          00684000
370   RC(J1)=0.0                                                        00684100
      GO TO 480                                                         00684200
380   RC(I1) =A(N)                                                      00684300
      IF (NARGS - 5) 480,480,390                                        00684400
390   RC(J1) = 1.0                                                      00684500
      GO TO 480                                                         00684600
400   M1 = 3*NRMAX + 2                                                  00684700
410   K2 = 0                                                            00684800
      DO 430 J=M1,M                                                     00684900
      IF (A(J) - A(J-1)) 420,430,430                                    00685000
420   AT = A(J)                                                         00685100
      A(J) = A(J-1)                                                     00685200
      A(J-1) =AT                                                        00685300
      N = J + NRMAX                                                     00685400
      AT = A(N)                                                         00685500
      A(N) = A(N-1)                                                     00685600
      A(N-1) = AT                                                       00685700
      K2 = K2 +1                                                        00685800
430   CONTINUE                                                          00685900
      IF (K2) 440,440,410                                               00686000
440   N = 4*NRMAX + 1                                                   00686100
450   RC(I1) = A(N)                                                     00686200
      I1= I1 + NROW                                                     00686300
      N = N +1                                                          00686400
      IF (I1 - I - IARGS(5) + 1) 450,450,460                            00686500
460   IF (NARGS - 5) 480,480,470                                        00686600
470   RC(J1) = M - 3*NRMAX                                              00686700
480   CONTINUE                                                          00686800
      GO TO 35                                                          00686900
C  SEARCH                                                               00687000
500   I1 = NARGS - 1                                                    00687100
      DO 520 I =1,NRMAX                                                 00687200
      K = IARGS(1) + I - 1                                              00687300
      L = IARGS(2) + I - 1                                              00687400
      M = NRMAX + I                                                     00687500
      A(I) = RC(L)                                                      00687600
      A(M) = RC(K)                                                      00687700
      J1 = 2                                                            00687800
      DO 510 N=3,I1,2                                                   00687900
      L= J1*NRMAX + I                                                   00688000
      M = IARGS(N) + I - 1                                              00688100
      A(L) = RC(M)                                                      00688200
510   J1 = J1 + 1                                                       00688300
      DO 520 N =4,NARGS,2                                               00688400
      M = IARGS(N) + I - 1                                              00688500
520   RC(M) = 0.0                                                       00688600
      K = NRMAX + 1                                                     00688700
      L = 2*NRMAX                                                       00688800
      DO 560 I=1,NRMAX                                                  00688900
      AT =  ABS(A(I)/1.E8)                                              00689000
      DO 550 J=K,L                                                      00689100
      IF ( ABS(A(I) - A(J)) - AT) 530,550,550                           00689200
530   J1=1                                                              00689300
      DO 540  N=4,NARGS,2                                               00689400
      M= IARGS(N) + I - 1                                               00689500
      I1= J1*NRMAX + J                                                  00689600
      RC(M) = A(I1)                                                     00689700
540   J1 =J1 + 1                                                        00689800
      GO TO 560                                                         00689900
550   CONTINUE                                                          00690000
560   CONTINUE                                                          00690100
      GO TO 35                                                          00690200
C  CENSOR                                                               00690300
600   DO 610 I=1,NRMAX                                                  00690400
      J = IARGS(1) + I -1                                               00690500
610   A(I) = RC(J)                                                      00690600
      DO 660 J=2,3                                                      00690700
      K = (J-1)*NRMAX                                                   00690800
      IF (KIND(J)) 640,620,640                                          00690900
620   DO 630 I=1,NRMAX                                                  00691000
      I1 = IARGS(J) + I - 1                                             00691100
      K = K + 1                                                         00691200
630   A(K) = RC(I1)                                                     00691300
      GO TO 660                                                         00691400
640   ARG3 = ARGS(J)                                                    00691500
      DO 650 I =1,NRMAX                                                 00691600
      K = K +1                                                          00691700
650   A(K) = ARG3                                                       00691800
660   CONTINUE                                                          00691900
      DO 680 I=1,NRMAX                                                  00692000
      J=   NRMAX + I                                                    00692100
      K= 2*NRMAX + I                                                    00692200
      L = IARGS(4) + I - 1                                              00692300
      IF (A(I) -  A(J)) 670,670,680                                     00692400
670   A(I) = A(K)                                                       00692500
680   RC(L) = A(I)                                                      00692600
      GO TO 35                                                          00692700
       END                                                              00692800
C  87   4      SUBROUTINE SEPINS                   2 19 68              00692900
      SUBROUTINE SEPINS                                                 00693000
      CALL X( "SEPINS" )                                                00693100
      RETURN                                                            00693200
      END                                                               00693300
C  88  52      SUBROUTINE SET                      2 19 68              00693400
      SUBROUTINE SET                                                    00693500
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00693600
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00693700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00693800
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00693900
      DIMENSION KFMT(17)                                                00694000
      DIMENSION NOUT(6)                                                 00694100
      DATA NOUT(1),NOUT(2),NOUT(3),NOUT(4),NOUT(5),NOUT(6) / 729, 1458, 00694200
     1 2187,2916, 3645, 4374 /                                          00694300
      DIMENSION ARGS(100)                                               00694400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00694500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00694600
      COMMON / QRS / NDROW, IFLAG, J, NNARG                             00694700
      ISRFLG = 1                                                        00694800
      IF ( NARGS .GE. 1 .AND. NARGS .LE. 3 ) GO TO 10                   00694900
      CALL ERROR( 10 )                                                  00695000
      GO TO 99                                                          00695100
  10  MODE = 2                                                          00695200
      DO 12 I=1,6                                                       00695300
      L2 = I+1                                                          00695400
      IF ( NAME(3) .EQ. NOUT(L2-1) ) GO TO 14                           00695500
   12 CONTINUE                                                          00695600
      GO TO 16                                                          00695700
   14 L1 = 0                                                            00695800
      CALL SETFMT( KFMT )                                               00695900
   16 CALL ADRESS( NARGS , J )                                          00696000
      IF( J ) 15, 17, 20                                                00696100
  15  CALL ERROR( 3 )                                                   00696200
      GO TO 99                                                          00696300
  17  CALL ERROR( 11 )                                                  00696400
      GO TO 99                                                          00696500
  20  NDROW = J + NROW - 1                                              00696600
      IF( NARGS .EQ. 1 ) GO TO 30                                       00696700
      IF ( L1 .EQ. 0 ) GO TO 200                                        00696800
      IF( KIND( 1 ) .NE. 0 ) GO TO 15                                   00696900
      IF( IARGS( 1 ) .LE. NROW .AND. IARGS( 1 ) .GT. 0 ) GO TO 25       00697000
      CALL ERROR( 16 )                                                  00697100
      GO TO 99                                                          00697200
  25  J = J + IARGS( 1 ) - 1                                            00697300
  30  IFLAG = 0                                                         00697400
      MODE = 2                                                          00697500
      GO TO 100                                                         00697600
  99  IFLAG = 1                                                         00697700
 100  RETURN                                                            00697800
  200 KROW = 1                                                          00697900
      IF ( NARGS .EQ. 3 ) KROW = IARGS(2)                               00698000
      J = J + KROW - 1                                                  00698100
      JEND = IARGS(1) + J - 1                                           00698200
      READ ( INUNIT , KFMT ) ( RC( I ),I=J,JEND)                        00698300
      NRMAX =MAX0(NRMAX, IARGS(1) + KROW - 1 )                          00698400
      GO TO 100                                                         00698500
      END                                                               00698600
C  89  24      SUBROUTINE SETFMT( KFMT )           2 19 68              00698700
      SUBROUTINE SETFMT( KFMT )                                         00698800
      DIMENSION KFMT( 17 )                                              00698900
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00699000
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00699100
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00699200
C                                                                       00699300
C    SETUP FORMAT STATEMENT                                             00699400
C                                                                       00699500
C                                                                       00699600
C     USE STANDARD FORMAT IF L2 = 1 OR IF REQUESTED FORMAT HAS NOT BEEN 00699700
C     SUPPLIED YET.                                                     00699800
C                                                                       00699900
      IF( L2 .NE. 1 .AND. IFMT( 1, L2-1 ) .NE. 0 ) GO TO 30             00700000
C     USE STANDARD FORMAT IF L2 = 1 OR IF REQUESTED FORMAT HAS NOT BEEN 00700100
C     SUPPLIED YET.                                                     00700200
C                                                                       00700300
      IF( L2 .NE. 1 .AND. IFMT( 1, L2-1 ) .NE. 0 ) GO TO 30             00700400
      DO 10 I = 1, 4                                                    00700500
  10  KFMT( I ) = IFMTX( I )                                            00700600
  20  RETURN                                                            00700700
  30  DO 40 I = 1, 17                                                   00700800
  40  KFMT( I ) = IFMT( I, L2-1 )                                       00700900
      GO TO 20                                                          00701000
      END                                                               00701100
C  90  28      SUBROUTINE SETQ                     2 19 68              00701200
      SUBROUTINE SETQ                                                   00701300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00701400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00701500
      DIMENSION ARGS(100)                                               00701600
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00701700
      COMMON / QRS / NDROW, IFLAG, J, NNARG                             00701800
C     CHECK IF END OF ROW HAS BEEN EXCEEDED PREVIOUSLY IN THIS SET.     00701900
      IF( IFLAG .NE. 0 .OR. NARGS .EQ. 0 ) GO TO 99                     00702000
C     J IS WHERE NEXT DATA ITEM IS TO GO IN COLUMN                      00702100
C     JJ IS WHERE LAST DATA ITEM OF THIS SET IS TO GO                   00702200
C     NDROW IS ADDRESS OF LAST ELEMENT OF ROW.                          00702300
      JJ = J + NARGS - 1                                                00702400
      IF( JJ .LE. NDROW ) GO TO 10                                      00702500
      CALL ERROR( 201 )                                                 00702600
      IFLAG = 1                                                         00702700
      IF( J .GT. NDROW ) GO TO 99                                       00702800
      JJ = NDROW                                                        00702900
  10  K = 1                                                             00703000
      DO 30 I = J, JJ                                                   00703100
      IF( KIND( K ) .EQ. 0 ) GO TO 20                                   00703200
      RC( I ) = ARGS( K )                                               00703300
      GO TO 30                                                          00703400
  20  RC( I ) = IARGS( K )                                              00703500
  30  K = K + 1                                                         00703600
      J = JJ + 1                                                        00703700
      NRMAX = MAX0( NRMAX, JJ - NDROW + NROW )                          00703800
  99  RETURN                                                            00703900
      END                                                               00704000
C  91  94      SUBROUTINE SORDER                   2 19 68              00704100
      SUBROUTINE SORDER                                                 00704200
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00704300
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00704400
      DIMENSION ARGS(100)                                               00704500
      EQUIVALENCE( ARGS(1), RC(10101) )                                 00704600
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00704700
      COMMON / SCRAT / A(10000),NS                                      00704800
C           SUBROUTINE BY CARLA MESSINA 221.04  JUNE 1967               00704900
C     L2=8 FOR SORT, L2=9 FOR ORDER, L2=14 FOR HEIRARCHY                00705000
C                                                                       00705100
C TYPE 1 IS      HEIRARCHY OF COL  ++, STORE IN COL ++                  00705200
C           HEIRARCHY GIVES THE ROW LOCATION OF THE SMALLEST NO. OF THE 00705300
C     THE FIRST COLUMN IN THE FIRST ROW OF THE SECOND COLUMN            00705400
C     THE ROW NO. OF THE SECOND LOWEST NO. OF THE FIRST COLUMN IS STORED00705500
C     IN THE SECOND ROW OF THE SECOND COLUMN, ..... THE ROW NO. OF THE  00705600
C     LARGEST NO. OF THE FIRST COL IS STORED IN THE NRMAX ROW OF THE 2ND00705700
C     COLUMN.  THE FIRST COLUMN IS UNCHANGED BY THIS COMMAND.           00705800
C TYPE 2 IS   ORDER COLUMNS ++,++,++, ETC                               00705900
C        ORDER PLACES EACH ONE OF THE GIVEN COLUMNS IN NUMERICALLY      00706000
C     INCREASING ORDER.                                                 00706100
C TYPE 3 IS   SORT COL ++ CARRY ALONG COLUMNS ++,++, ETC                00706200
C        SORT PLACES THE FIRST COLUMN IN NUMERICALLY INCREASING ORDER   00706300
C     WHILE PRESERVING THE ROW RELATIONSHIPS AMONG THE GIVEN COLUMNS    00706400
C                                                                       00706500
C        THESE INSTRUCTIONS CAN BE DONE FASTER IF A MACHINE LANGUAGE    00706600
C     PROGRAM IS SUBSTITUTED FOR THIS ONE.                              00706700
C                                                                       00706800
      IF (NARGS) 10,10,40                                               00706900
10    K=10                                                              00707000
20    CALL ERROR(K)                                                     00707100
30    RETURN                                                            00707200
40    CALL CHKCOL(J)                                                    00707300
      IF (J) 50,60,50                                                   00707400
50    K=3                                                               00707500
      GO TO 20                                                          00707600
  60  IF( L2 - 9 ) 80, 80, 70                                           00707700
70    IF (NARGS-2) 10,80,10                                             00707800
80    IF (NERROR) 30,90,30                                              00707900
90    IF (NRMAX-1) 100,110,120                                          00708000
100   K=9                                                               00708100
      GO TO 20                                                          00708200
 110  IF( L2 - 9 ) 30, 30, 215                                          00708300
120   K3=1                                                              00708400
      K = IARGS(1) -1                                                   00708500
130   DO 140 I =1,NRMAX                                                 00708600
      J=K+I                                                             00708700
      L = NRMAX + I                                                     00708800
      A(I) = RC(J)                                                      00708900
140   A(L) = I                                                          00709000
150   K1 = NRMAX                                                        00709100
160   K1 = K1 -1                                                        00709200
      K2=0                                                              00709300
      IF (K1-1) 170,170,180                                             00709400
170   K1 = 2                                                            00709500
180   DO 200 I=1,K1                                                     00709600
      IF (A(I)-A(I+1)) 200,200,190                                      00709700
190   CC = A(I)                                                         00709800
      A(I) = A(I+1)                                                     00709900
      A(I+1) = CC                                                       00710000
      L=NRMAX + I                                                       00710100
      CC = A(L)                                                         00710200
      A(L) = A(L+1)                                                     00710300
      A(L+1) = CC                                                       00710400
      K2=1                                                              00710500
200   CONTINUE                                                          00710600
      IF (K2) 160,210,160                                               00710700
 210  IF( L2 - 9 ) 240, 240, 220                                        00710800
215   A(NRMAX+1)=1.0                                                    00710900
220   K= IARGS(2) - 1                                                   00711000
      DO 230 I=1,NRMAX                                                  00711100
      J= K+ I                                                           00711200
      L=NRMAX + I                                                       00711300
230   RC(J) = A(L)                                                      00711400
      GO TO 30                                                          00711500
240   DO 250 I=1,NRMAX                                                  00711600
      J= K+ I                                                           00711700
250   RC(J) = A(I)                                                      00711800
      IF (NARGS-2) 30,260,260                                           00711900
 260  IF( L2 - 9 ) 290, 270, 270                                        00712000
270   IF (NARGS-K3) 30,30,280                                           00712100
280   K3 = K3 + 1                                                       00712200
      K = IARGS(K3) - 1                                                 00712300
      GO TO 130                                                         00712400
290   DO 310 I =2,NARGS                                                 00712500
      K = IARGS(I) - 1                                                  00712600
      DO 300 J=1,NRMAX                                                  00712700
      L = NRMAX + J                                                     00712800
      J1 = A(L) + K                                                     00712900
300   A(J) = RC(J1)                                                     00713000
      DO 310 J=1,NRMAX                                                  00713100
      J1= K + J                                                         00713200
310   RC(J1) = A(J)                                                     00713300
      GO TO 30                                                          00713400
      END                                                               00713500
C  92  40      SUBROUTINE SORTSM(N,SUM)            2 19 68              00713600
      SUBROUTINE SORTSM(N,SUM)                                          00713700
C *****                                                                 00713800
C     SORT COLUMN OF PRODUCTS FOR MATRIX MULTIPLICATION                 00713900
C     AFTER SORTING START SUMMING BEGIN IN MIDDLE OF SORTED COLUMN      00714000
C *****                                                                 00714100
      COMMON/MULTC/NS2                                                  00714200
      COMMON / SCRAT / X,NS                                             00714300
      DIMENSION A(10000)                                                00714400
      DOUBLE PRECISION X(5000), SUM, SAVE                               00714500
                                                                        00714600
      IF ( N .NE. 1 ) GO TO 80                                          00714700
      SUM = X( NS2 )                                                    00714800
      RETURN                                                            00714900
   80 SUM = X(NS2)                                                      00715000
      IS = NS2 - 1                                                      00715100
      DO 120  I=2,N                                                     00715200
      SUM = SUM + X(IS)                                                 00715300
 120  IS=IS-1                                                           00715400
      RETURN                                                            00715500
      END                                                               00715600
C  93  24      SUBROUTINE SPACE                    2 19 68              00715700
      SUBROUTINE SPACE                                                  00715800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00715900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00716000
      DIMENSION ARGS(100)                                               00716100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00716200
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00716300
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00716400
      IF( NARGS - 1 ) 60, 40, 10                                        00716500
  10  I = 10                                                            00716600
  20  CALL ERROR( I )                                                   00716700
  30  RETURN                                                            00716800
  40  IF ( KIND(1) .EQ. 0 ) IF ( IARGS(1) ) 50,30,70                    00716900
      I = 20                                                            00717000
      GO TO 20                                                          00717100
  50  I = 3                                                             00717200
      GO TO 20                                                          00717300
  60  IARGS( 1 ) = 1                                                    00717400
  70  J = MIN0( 60, IARGS( 1 ) )                                        00717500
      IF( NERROR .NE. 0 ) GO TO 30                                      00717600
      DO 80 I = 1, J                                                    00717700
  80  WRITE( IPRINT, 90 )                                               00717800
  90  FORMAT(1X)                                                        00717900
      GO TO 30                                                          00718000
      END                                                               00718100
C  94  84      SUBROUTINE SPINV(A,M,KK,ISIG)       2 19 68              00718200
      SUBROUTINE SPINV(A,M,KK,ISIG)                                     00718300
C7058MI  MATRIX INVERSION WITH MINIMUM ROUNDOFF ERROR ACCUMULATION. PREC00718400
      DATA ONE/1.0/,ZERO/0.0/,ER/1.E-8/                                 00718500
      DIMENSION A(1)                                                    00718600
      ISIG = 0                                                          00718700
      N = M                                                             00718800
      NN = KK                                                           00718900
      N2 = N + N                                                        00719000
      DO 10 J=1,N                                                       00719100
      NJCOL = (N + J - 1) * NN                                          00719200
      DO 10 I=1,N                                                       00719300
      KINJ = NJCOL + I                                                  00719400
      IF(I-J)4,6,4                                                      00719500
    4 A(KINJ)=ZERO                                                      00719600
      GO TO 10                                                          00719700
    6 A(KINJ) = ONE                                                     00719800
   10 CONTINUE                                                          00719900
C  DETERMINE MAXIMUM ABS OF VARIABLE BEING ELIMINATED.  THIS BECOMES PIV00720000
      L = 0                                                             00720100
   12 L = L + 1                                                         00720200
      LCOL = NN*L-NN                                                    00720300
      KLL = LCOL + L                                                    00720400
      IF(L - N)13,30,1000                                               00720500
C  FIND THE LARGEST ELEMENT IN THE LTH COLUMN.                          00720600
   13 J1 = L                                                            00720700
      C = ABS ( A(KLL) )                                                00720800
      L1 = L + 1                                                        00720900
      DO 20 I = L1,N                                                    00721000
      KIL = LCOL + I                                                    00721100
      X = ABS (A(KIL))                                                  00721200
      IF(C - X)14,20,20                                                 00721300
C  RECORD THE NUMBER OF THE ROW HAVING THE GREATER ELEMENT.             00721400
   14 J1 = I                                                            00721500
C  C BECOMES THE GREATER.                                               00721600
      C  = X                                                            00721700
   20 CONTINUE                                                          00721800
C  INTERCHANGE ROW J1 WITH ROW L. J1 IS THE ROW WITH THE LARGEST ELEMENT00721900
C  TEST TO SEE IF INTERCHANGING IS NECESSARY.                           00722000
      IF(J1 - L)22,30,22                                                00722100
   22 DO 24 J = L,N2                                                    00722200
      JCOL = NN*J-NN                                                    00722300
      KJIJ = JCOL + J1                                                  00722400
      HOLD = A(KJIJ)                                                    00722500
      KLJ = JCOL + L                                                    00722600
      A(KJIJ) = A(KLJ)                                                  00722700
      A(KLJ) = HOLD                                                     00722800
   24 CONTINUE                                                          00722900
C  IF  THE LARGEST ABSOLUTE ELEMENT IN A COLUMN IS ZERO WE HAVE A SINGUL00723000
  30  IF(ABS(A(KLL))-ER)33,33,32                                        00723100
  33  ISIG = 4                                                          00723200
      GO TO 1000                                                        00723300
C   ZERO ALL THE ELEMENTS IN THE LTH COLUMN BUT THE PIVOTAL ELEMENT.    00723400
   32 L1 = 1                                                            00723500
      L2 = L - 1                                                        00723600
      IF(L2)321,321,323                                                 00723700
  321 IF(L-N)322,46,322                                                 00723800
  322 L1 = L + 1                                                        00723900
      L2 = N                                                            00724000
  323 DO 324 I = L1,L2                                                  00724100
      KIL = LCOL + I                                                    00724200
      Z = -A(KIL)/A(KLL)                                                00724300
      DO 324 J = L,N2                                                   00724400
      JCOL = NN*J - NN                                                  00724500
      KIJ = JCOL + I                                                    00724600
      KLJ = JCOL + L                                                    00724700
  324 A(KIJ) = A(KIJ) + Z*A(KLJ)                                        00724800
      IF(N - L2)12,12,321                                               00724900
C  DIVIDE BY DIAGONAL ELEMENTS.                                         00725000
   46 DO 48 I = 1,N                                                     00725100
      KKK = NN*I - NN + I                                               00725200
      ZZ = A(KKK)                                                       00725300
      DO 48 J = 1,N2                                                    00725400
      KKI = NN*J - NN + I                                               00725500
   48 A(KKI) = A(KKI)/ZZ                                                00725600
C   RETURN AFTER PUTTING A INVERSE INTO B                               00725700
   49 DO 50 J = 1,N                                                     00725800
      JCOL = NN*J - NN                                                  00725900
      NJCOL = NN * N + JCOL                                             00726000
      DO 50 I = 1,N                                                     00726100
      KIJ = JCOL + I                                                    00726200
      KINJ = NJCOL + I                                                  00726300
  50  A(KIJ) = A(KINJ)                                                  00726400
 1000 RETURN                                                            00726500
       END                                                              00726600
C  95 449      SUBROUTINE  STATIS                  2 19 68              00726700
      SUBROUTINE  STATIS                                                00726800
C     PROGRAM WRITTEN BY S. PEAVY   8/31/67                             00726900
C**** OMNITAB COMMAN IS AS FOLLOWS                                      00727000
C**** I  WITH WEIGHTS                                                   00727100
C****  A. STATIS  COL +++  WEIGHTS +++  START STORING RESULTS +++       00727200
C****      (RESULTS WILL BE STORED IN THE NEXT 4 COL)                   00727300
C****  B. STATIS  COL +++ WHTS +++ RESULTS +++,+++,+++,+++              00727400
C**** II WITHOUT WHTS                                                   00727500
C****  A. SAME AS I. A.  EXCEPT WHTS COL OMITTED                        00727600
C****  B. SAME AS I. B.  EXCEPT WHTS COL OMITTED                        00727700
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00727800
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00727900
      COMMON / SCRAT / A(10000),NS                                      00728000
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00728100
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00728200
      DIMENSION ARGS(100)                                               00728300
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00728400
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00728500
      DIMENSION  SA(1250,3),ISA(1250)                                   00728600
      DIMENSION IB(10)                                                  00728700
      EQUIVALENCE (A(101),ISA),(A(1351),SA),(NRMAX,NARMAX)              00728800
      DIMENSION  BCON(4),BKCON(4),AKCON(4),AT5(6),CK1(6),DK2(6),XK1(7), 00728900
     1 YK2(7)                                                           00729000
      DATA BCON/3.6948,-1.6561,.406,2.7764/,BKCON/7.45894,-.89082,      00729100
     1 .61522,2.56706/,AKCON/-.51732,-.61863,-.04122,.55897/,AT5/       00729200
     2 1.9599640,2.3722712,2.8224986,2.5558497,1.5895341,.7328982/,CK1/ 00729300
     3 -.70285,-.02006,-.01687,-.01447,-.01263,.67839/,DK2/-1.49016,    00729400
     4 .13384,.09764,.07476,.05931,1.68641/,XK1/-40.343875,14.1365,     00729500
     5 -2.743342,.84143957,.001066,-6.3701507E-6,1.749484E-8/,YK2/      00729600
     6 50.298233,-11.395210,6.0537922,1.1542370,-9.8051279E-4,5.5609437E00729700
     7 -6,1.4584433E-8/,CONK/1.959964/                                  00729800
      DATA ZERO/0.0/,ONE/1.0/,TWO/2.0/                                  00729900
      NXCOL=IARGS(1)                                                    00730000
      NXWT= IARGS(2)                                                    00730100
      ISTORE=1                                                          00730200
      NAR=NARGS                                                         00730300
      IWT=1                                                             00730400
      IF(NARGS.EQ.1) GO TO 80                                           00730500
      IF(NARGS.EQ.3.AND.IARGS(NARGS).LT.0) GO TO 70                     00730600
      GO TO 90                                                          00730700
   70 NARGS=NARGS-1                                                     00730800
      IWT=2                                                             00730900
   80 ISTORE=2                                                          00731000
      GO TO  95                                                         00731100
   90 IF(NARGS.NE.2.AND.NARGS.NE.3.AND.NARGS.NE .5.AND.NARGS.NE.6)      00731200
     1CALL ERROR (10)                                                   00731300
  95  J=NARGS                                                           00731400
      CALL CKIND(J)                                                     00731500
      IF(J    .NE.0) CALL ERROR (3)                                     00731600
      CALL CHKCOL(J)                                                    00731700
      IF(J.NE.0) CALL ERROR(11)                                         00731800
      IF(NARMAX*4.GT.NS  ) CALL ERROR (225)                             00731900
      IF(NERROR.NE.0) RETURN                                            00732000
      IXN=NRMAX                                                         00732100
      A(1)=NRMAX                                                        00732200
      K=IARGS(1)                                                        00732300
      M=1                                                               00732400
      IF(NAR  .EQ.3.OR.NAR  .EQ.6) GO TO  120                           00732500
      NZW=NRMAX                                                         00732600
      SUM=0.0                                                           00732700
      S2=0.                                                             00732800
      WT=0.0                                                            00732900
      DO  100 I=1,IXN                                                   00733000
      SA(I,2)=RC(K)                                                     00733100
      SA(I,3)=1.0                                                       00733200
      ISA(I)=M                                                          00733300
      SA(I,1)=RC(K)                                                     00733400
      K=K+1                                                             00733500
      M=M+1                                                             00733600
      WT=WT+1.                                                          00733700
      SUM=SUM+SA(I,2)                                                   00733800
  100 S2=S2+SA(I,2)**2                                                  00733900
      SUMWT=SUM                                                         00734000
      GO TO 150                                                         00734100
  120 SUM=0.                                                            00734200
      WT=0.                                                             00734300
      SUMWT=0.0                                                         00734400
      MA=IARGS(2)                                                       00734500
      S2=0.0                                                            00734600
      IWT=2                                                             00734700
      DO  130 I=1,IXN                                                   00734800
      IF(RC(MA).EQ.0.)GO TO 125                                         00734900
      SA(M,2)=RC(K)                                                     00735000
      SA(M,3)=RC(MA)                                                    00735100
      ISA(M)=M                                                          00735200
      SA(M,1)=RC(K)                                                     00735300
      S2=S2+SA(M,2)**2*RC(MA)                                           00735400
      SUM=SUM+RC(K)                                                     00735500
      WT=WT+RC(MA)                                                      00735600
      SUMWT=SA(M,2)*RC(MA)+SUMWT                                        00735700
      M=M+1                                                             00735800
  125 K=K+1                                                             00735900
  130 MA=MA+1                                                           00736000
      NZW=M-1                                                           00736100
  150 A(2)=NZW                                                          00736200
      A(3)=SUM/A(2)                                                     00736300
      A(4)=SUMWT/WT                                                     00736400
      A(24)=(2*A(2)-1.)/3.                                              00736500
      A(25)=FSQRT((16.*A(2)-29.)/90.)                                   00736600
      A(39)=S2                                                          00736700
      IXN=NZW                                                           00736800
      IXNM1=IXN-1                                                       00736900
  405 IST=0                                                             00737000
      DO 410 I=2,IXN                                                    00737100
      IF(SA(I-1,1).LE.SA(I,1)) GO TO 410                                00737200
      K=ISA(I-1)                                                        00737300
      ISA(I-1)=ISA(I)                                                   00737400
      ISA(I)=K                                                          00737500
      T=SA(I-1,1)                                                       00737600
      SA(I-1,1)=SA(I,1)                                                 00737700
      SA(I,1)=T                                                         00737800
      IST=1                                                             00737900
  410 CONTINUE                                                          00738000
      IF(IST.NE.0) GO TO 405                                            00738100
      NALPHA=.05*A(2)                                                   00738200
      IXA=NALPHA+1                                                      00738300
      IXNA=IXN-NALPHA                                                   00738400
      TSUM=0.                                                           00738500
      TWSUM=0                                                           00738600
      TWT=0                                                             00738700
      DO 660  I=IXA,IXNA                                                00738800
      M=ISA(I)                                                          00738900
      TWSUM=TWSUM+SA(I,1)*SA(M,3)                                       00739000
      TWT=TWT+SA(M,3)                                                   00739100
  660 TSUM=TSUM+SA(I,1)                                                 00739200
      A(7)=TSUM/(A(2)-2.*FLOAT(NALPHA))                                 00739300
      A(8)=TWSUM/TWT                                                    00739400
      N2=(NZW+1)/2                                                      00739500
      A(5)=SA(N2,1)                                                     00739600
      IF(MOD(NZW,2).EQ.0) A(5)=(A(5)+SA(N2+1,1))/TWO                    00739700
      A(6)=(SA(1,1)+SA(IXN,1))/TWO                                      00739800
      A(11)=SA(IXN,1)-SA(1,1)                                           00739900
      A(34)=SA(1,1)                                                     00740000
      A(35)=SA(IXN,1)                                                   00740100
      DELX=A(11)/10.                                                    00740200
      XB=SA(1,1)                                                        00740300
      XT=XB+DELX                                                        00740400
      L=1                                                               00740500
      DO 520  I=1,10                                                    00740600
      IC=0                                                              00740700
  500 IF(SA(L,1).GE.XT) GO TO 510                                       00740800
      IC=IC+1                                                           00740900
      L=L+1                                                             00741000
      IF(L.NE.IXN) GO TO 500                                            00741100
  510 A(I+50)=IC                                                        00741200
  520 XT=XT+DELX                                                        00741300
      IF(L.GT.IXN) GO TO 527                                            00741400
      DO 524 I=L,IXN                                                    00741500
      IF(SA(I  ,1).GE.XT-DELX)  A(60)=A(60)+1.                          00741600
  524 CONTINUE                                                          00741700
  527 DO  530 I=1,IXNM1                                                 00741800
  530 SA(I,3)=SA(I+1,1)-SA(I,1)                                         00741900
      LA=1                                                              00742000
      DO 420 I=1,IXN                                                    00742100
      K=ISA(I)                                                          00742200
      SA(K,1)=LA                                                        00742300
  420 LA=LA+1                                                           00742400
      K=0                                                               00742500
      RNS=0.                                                            00742600
      RNSS=ONE                                                          00742700
      LR=0                                                              00742800
      DO 470 I=1,IXNM1                                                  00742900
      IF(SA(I,3).NE.0.AND.K.EQ.0) GO TO 460                             00743000
      IF(SA(I,3).NE.0) GO TO 430                                        00743100
      RNS=RNS+RNSS                                                      00743200
      K=K+1                                                             00743300
      GO TO 470                                                         00743400
  430 K=K+1                                                             00743500
      RNS=RNS+RNSS                                                      00743600
      RNS=RNS/FLOAT(K)                                                  00743700
      DO 450 L=1,K                                                      00743800
      LR=LR+1                                                           00743900
      LRR=ISA(LR)                                                       00744000
  450 SA(LRR,1)=RNS                                                     00744100
      LR=LR-1                                                           00744200
      RNS=0.                                                            00744300
      K=0                                                               00744400
  460 LR=LR+1                                                           00744500
  470 RNSS=RNSS+ONE                                                     00744600
      ICI=0                                                             00744700
      IPLUS=0                                                           00744800
      IMINUS=0                                                          00744900
      IDRUNS=0                                                          00745000
      IC=0                                                              00745100
      ADEV=0.0                                                          00745200
      DEV3=0.0                                                          00745300
      DEV2=0.0                                                          00745400
      DEV=0.0                                                           00745500
      DEVI=0.0                                                          00745600
      DEVWT=0.                                                          00745700
      DEV4=0.0                                                          00745800
      AK=1.                                                             00745900
      KWT=IARGS(2)                                                      00746000
      NRXX=KWT+NRMAX-1                                                  00746100
      TA=1.0                                                            00746200
      DO  250 I=1,IXN                                                   00746300
      T=SA(I,2)-A(4)                                                    00746400
      SA(I,3)=T                                                         00746500
      DEV=T+DEV                                                         00746600
      ADEV=ADEV+ABS(T)                                                  00746700
      DEV2=DEV2+T**2                                                    00746800
      DEV3=DEV3+T**3                                                    00746900
      DEV4=DEV4+T**4                                                    00747000
      DEVI=AK*T+DEVI                                                    00747100
      AK=AK+1.0                                                         00747200
      IF(IWT.EQ.1) GO TO 210                                            00747300
  203 IF(RC(KWT).NE.0.)GO TO 204                                        00747400
      IF(KWT.GE.NRXX ) GO TO 200                                        00747500
      KWT=KWT+1                                                         00747600
      GO TO 203                                                         00747700
  204 TA=RC(KWT)                                                        00747800
  210 DEVWT=DEVWT+TA*T**2                                               00747900
  200 IF(T.LT.0) GO TO 230                                              00748000
      IPLUS=IPLUS+1                                                     00748100
      ICI=+1                                                            00748200
      GO TO  240                                                        00748300
  230 IMINUS=IMINUS+1                                                   00748400
      ICI=-1                                                            00748500
  240 IF(IC.EQ.ICI) GO TO 250                                           00748600
      IC=ICI                                                            00748700
      IDRUNS=IDRUNS+1                                                   00748800
  250 KWT=KWT+1                                                         00748900
      A(13)=DEVWT/(A(2)-1.)                                             00749000
      A(9)=FSQRT(A(13))                                                 00749100
      A(10)=A(9)/FSQRT(WT)                                              00749200
      A(14)=100.*A(9)/A(4)                                              00749300
      A(28)=IPLUS                                                       00749400
      A(29)=IMINUS                                                      00749500
      A(31)=1.+(2.*A(28)*A(29)/A(2))                                    00749600
      A(32)=FSQRT((2.*A(28)*A(29)*(2.*A(28)*A(29)-A(28)-A(29))) /       00749700
     1( (A(28)+A(29))**2*(A(2)-1.)))                                    00749800
      A(36)=(DEV3/A(2))**2/( (A(2)-1.)/A(2)*A(13))**3                   00749900
      A(37)=(DEV4/A(2))/( (A(2)-1.) /A(2)*A(13))**2                     00750000
      A(38)=SUMWT                                                       00750100
      A(40)=DEVWT                                                       00750200
      A(30)=IDRUNS                                                      00750300
      A(33)=(A(30)-A(31))/A(32)                                         00750400
      A(19)=12.*DEVI/(A(2)*(A(2)**2-1.))                                00750500
      A(20)=FSQRT((1./(A(2)-2.))*(12.*DEV2/(A(2)*(A(2)**2-1.))-A(19)**2)00750600
     1)                                                                 00750700
      A(21)=A(19)/A(20)                                                 00750800
      CALL PROB(ONE,A(2)-ONE ,A(21)*A(21),A(22))                        00750900
      DIF=0                                                             00751000
      IRUN=1                                                            00751100
      TA=SA(2,2)-SA(1,2)                                                00751200
      DO 300 I=2,IXN                                                    00751300
      T=SA(I,2)-SA(I-1,2)                                               00751400
      DIF=DIF+T**2                                                      00751500
      IF (TA*T.GE.0) GO TO 300                                          00751600
      TA=T                                                              00751700
      IRUN=IRUN+1                                                       00751800
  300 CONTINUE                                                          00751900
      A(23)=IRUN                                                        00752000
      A(26)=DIF/(A(2)-1.)                                               00752100
      A(27)=A(26)/A(13)                                                 00752200
      A(41)=A(4)*FSQRT(WT)/A(9)                                         00752300
      A(12)=ADEV /A(2)                                                  00752400
      NU=NZW-1                                                          00752500
      VNU=NU                                                            00752600
      T=ZERO                                                            00752700
      TK1=ZERO                                                          00752800
      TK2=ZERO                                                          00752900
      IF(NU.GE.5)  GO TO 1210                                           00753000
      DO  1200 I=1,4                                                    00753100
      V=I/NU                                                            00753200
      T=T+BCON(I)*V                                                     00753300
      TK2=BKCON(I)*V+TK2                                                00753400
 1200 TK1=TK1+AKCON(I)*V                                                00753500
      GO TO 1260                                                        00753600
 1210 T= (((( AT5(6)/VNU+AT5(5))/VNU+AT5(4))/VNU+AT5(3))/VNU+AT5(2))/VNU00753700
     1 +AT5(1)                                                          00753800
      IF (NU.GT. 10 ) GO TO 1230                                        00753900
      DO  1220  I=1,6                                                   00754000
      V=(I+4)/NU                                                        00754100
      TK1=TK1+CK1(I)*V                                                  00754200
 1220 TK2=TK2+DK2(I)*V                                                  00754300
      GO TO 1260                                                        00754400
 1230 IF(NU.GT.100) GO TO 1250                                          00754500
      DO  1240 I=1,7                                                    00754600
      V=VNU**(I-4)                                                      00754700
      TK1=TK1+XK1(I)*V                                                  00754800
 1240 TK2=TK2+YK2(I)*V                                                  00754900
      GO TO 1260                                                        00755000
 1250 V2=FSQRT(TWO*VNU)                                                 00755100
      V2M1=FSQRT(TWO*VNU-ONE)                                           00755200
      TK1=V2/(CONK  +V2M1)                                              00755300
      TK2=V2/(-CONK+V2M1)                                               00755400
 1260 A(15)=A(4)-T*A(10)                                                00755500
      A(16)=A(4)+T*A(10)                                                00755600
      A(17)=TK1*A(9)                                                    00755700
      A(18)=TK2*A(9)                                                    00755800
C**** START PRINT OUT                                                   00755900
      IF(L2.EQ.2) GO TO 930                                             00756000
      CALL PAGE (4)                                                     00756100
      IF(IWT.EQ.2) GO TO 760                                            00756200
      WRITE(IPRINT,1000)NXCOL,NZW                                       00756300
      GO TO 785                                                         00756400
  760 IF(NZW.NE.NRMAX) GO TO 770                                        00756500
      WRITE(IPRINT,1010)  NXCOL,NXWT,NZW                                00756600
      GO TO 780                                                         00756700
  770 WRITE(IPRINT,1020) NXCOL,NXWT,NZW,NRMAX                           00756800
  780 WRITE (IPRINT,1030)                                               00756900
  785 DO  790  I=1,10                                                   00757000
  790 IB(I)=A(I+50)                                                     00757100
      WRITE(IPRINT,1040) (IB(I),I=1,10)                                 00757200
      WRITE(IPRINT,1050)                                                00757300
      WRITE(IPRINT,1060)  ( A(I+2),A(I+8),I=1,6)                        00757400
      WRITE(IPRINT,1070)  (A(I),I=15,18)                                00757500
      WRITE(IPRINT,1080)  (A(I),A(I+15),I=19,22),(A(I),I=38,41)         00757600
      IB(1)=A(23)                                                       00757700
      IB(2)=A(28)                                                       00757800
      IB(3)=A(29)                                                       00757900
      IB(4)=A(30)                                                       00758000
      WRITE(IPRINT,1090) IB(1),(A(I),I=24,27),(IB(I),I=2,4),            00758100
     1(A(I),I=31,33)                                                    00758200
      WRITE(IPRINT,1100)                                                00758300
      KB=ISA(1)                                                         00758400
      T=SA(KB,2)                                                        00758500
      LINEP=40                                                          00758600
      LINE=0                                                            00758700
      LW=IARGS(2)                                                       00758800
      DO  870 I=1,IXNM1                                                 00758900
      IF(LINEP.LT.40) GO TO 810                                         00759000
      LINEP=0                                                           00759100
      CALL PAGE (4)                                                     00759200
      WRITE(IPRINT,1110)                                                00759300
      IF(IWT.EQ.1) GO TO 800                                            00759400
      WRITE(IPRINT,1120)                                                00759500
      GO TO 810                                                         00759600
  800 WRITE(IPRINT,1130)                                                00759700
  810 K=ISA(I+1)                                                        00759800
      TA=SA(K,2)-T                                                      00759900
      GO TO (850,840),IWT                                               00760000
  840 IF(RC(LW).NE.0) GO TO 845                                         00760100
      LW=LW+1                                                           00760200
      GO TO 840                                                         00760300
  845 WRITE(IPRINT,1150) I,SA(I,2),SA(I,1),SA(I,3),RC(LW),ISA(I),T,TA   00760400
      LW=LW+1                                                           00760500
      GO TO 860                                                         00760600
  850 WRITE(IPRINT,1140)I,SA(I,2),SA(I,1),SA(I,3),ISA(I),T,TA           00760700
  860 T=SA(K,2)                                                         00760800
      LINE=LINE+1                                                       00760900
      IF(LINE.NE.10) GO TO 870                                          00761000
      LINE=0                                                            00761100
      LINEP=LINEP+10                                                    00761200
      WRITE(IPRINT,1160)                                                00761300
  870 CONTINUE                                                          00761400
      IF(IWT.EQ.1) GO TO 920                                            00761500
  900 IF(RC(LW).NE.0) GO TO 910                                         00761600
      LW=LW+1                                                           00761700
      GO TO 900                                                         00761800
  910 WRITE (IPRINT,1150) NZW,SA(NZW,2),SA(NZW,1),SA(NZW,3),RC(LW),     00761900
     1 ISA(NZW),T                                                       00762000
      GO TO 930                                                         00762100
  920 WRITE(IPRINT,1140) NZW,SA(NZW,2),SA(NZW,1),SA(NZW,3),ISA(NZW),T   00762200
  930 IF(ISTORE.EQ.2) RETURN                                            00762300
      IF(NARGS.EQ.2.OR.NARGS.EQ.3) GO TO 940                            00762400
      L=IARGS(NARGS-3)                                                  00762500
      M=IARGS(NARGS-2)                                                  00762600
      K=IARGS(NARGS-1)                                                  00762700
      J=IARGS(NARGS)                                                    00762800
      GO TO 950                                                         00762900
  940 L=IARGS(NARGS)                                                    00763000
      M=L+NROW                                                          00763100
      K=M+NROW                                                          00763200
      J=K+NROW                                                          00763300
  950 DO  960 I=1,NZW                                                   00763400
      MB=ISA(I)                                                         00763500
      RC(K)=SA(MB,2)                                                    00763600
      RC(M)=SA(I,1)                                                     00763700
      RC(J)=SA(I,3)                                                     00763800
      M=M+1                                                             00763900
      K=K+1                                                             00764000
  960 J=J+1                                                             00764100
      IF(NZW.EQ.NRMAX) GO TO 975                                        00764200
      NZW1=NZW+1                                                        00764300
      DO 970  I=NZW1,NRMAX                                              00764400
      RC(M)=0.                                                          00764500
      RC(K)=0.                                                          00764600
      RC(J)=0.                                                          00764700
      M=M+1                                                             00764800
      K=K+1                                                             00764900
  970 J=J+1                                                             00765000
 975  NTOP=60                                                           00765100
      IF ( NROW .LT. NTOP ) NTOP = NROW                                 00765200
      DO   980  I=1,NTOP                                                00765300
      RC(L)=A(I)                                                        00765400
  980 L=L+1                                                             00765500
      IF(NRMAX.LT.60) RETURN                                            00765600
      DO 990  I=61,NRMAX                                                00765700
      RC(L)=0.                                                          00765800
  990 L=L+1                                                             00765900
      RETURN                                                            00766000
 1000 FORMAT(1H0,4X,28HSTATISTICAL ANALYSIS OF COL ,I4,33X,4HN = ,I4)   00766100
 1010 FORMAT(1H0,4X,28HSTATISTICAL ANALYSIS OF COL ,I4,8X,15HWEIGHTS IN 00766200
     1COL ,I4,6X,4HN = ,I4)                                             00766300
 1020 FORMAT(1H0,4X,28HSTATISTICAL ANALYSIS OF COL ,I4,8X,15HWEIGHTS IN 00766400
     1COL ,I4,6X,4HN = ,I4,33H(NO OF NON-ZERO WTS) COL LENGTH =,I4)     00766500
 1030 FORMAT(1H0,24X,64HALL COMPUTATIONS ARE BASED ON OBSERVATIONS WITH 00766600
     1NON-ZERO WEIGHTS )                                                00766700
 1040 FORMAT(1H0/15X,28HFREQUENCY DISTRIBUTION (1-6),7X,10I6)           00766800
 1050 FORMAT(1H0/5X, 26HMEASURES OF LOCATION (2-2),34X,28HMEASURES OF DI00766900
     1SPERSION (2-6))                                                   00767000
 1060 FORMAT(1H0,                                                       00767100
     1 9X,26HUNWEIGHTED MEAN          =,   1PE15.7,20X,                 00767200
     2    26HSTANDARD DEVIATION       =,     E15.7    /                 00767300
     310X,26HWEIGHTED MEAN            =,     E15.7,20X,                 00767400
     4    26HS.D. OF MEAN             =,     E15.7    /                 00767500
     510X,26HMEDIAN                   =,     E15.7,20X                  00767600
     6    26HRANGE                    =,     E15.7    /                 00767700
     710X,26HMID-RANGE                =,     E15.7,20X,                 00767800
     8    26HMEAN DEVIATION           =,     E15.7    /                 00767900
     910X,26H5 PCT UNWTD TRIMMED MEAN =,     E15.7,20X                  00768000
     A    26HVARIANCE                 =,     E15.7    /                 00768100
     B10X,26H5 PCT WTD TRIMMED MEAN   =,      E15.7,20X                 00768200
     C    26HCOEFFICIENT OF VARIATION =,     E15.7    )                 00768300
 1070 FORMAT (1H0//                                                     00768400
     120X,50HA TWO-SIDED 95 PCT CONFIDENCE INTERVAL FOR MEAN IS 1PE11.4,00768500
     2 3H TO,E11.4, 6H (2-2)/                                           00768600
     320X,50HA TWO-SIDED 95 PCT CONFIDENCE INTERVAL FOR S.D. IS, E11.4, 00768700
     4 3H TO,E11.4, 6H (2-7))                                           00768800
 1080 FORMAT                                                            00768900
     1(1H0//5X,30HLINEAR TREND STATISTICS (5-1) ,30X,16HOTHER STATISTICS00769000
     2//10X,5HSLOPE,20X,1H=,1PE15.7,20X,7HMINIMUM,18X,1H=,E15.7/        00769100
     3  10X,13HS.D. OF SLOPE,12X,1H=,E15.7,20X,7HMAXIMUM,18X,1H=,E15.7/ 00769200
     4 10X,26HSLOPE/S.D. OF SLOPE = T  =,E15.7,20X,8HBETA ONE,17X,1H=,  00769300
     5 E15.7/10X,35HPROB EXCEEDING ABS VALUE OF OBS T =,0PF6.3,20X,     00769400
     6 8HBETA TWO,17X,1H=,1PE15.7/71X,17HWTD SUM OF VALUES,8X,1H=,E15.7/00769500
     7  71X,18HWTD SUM OF SQUARES,7X,1H=,E15.7/5X,24HTESTS FOR NON-RANDO00769600
     8MNESS,42X,26HWTD SUM OF DEVS SQUARED  =,E15.7/71X,11HSTUDENT"S T, 00769700
     9 14X,1H=,E15.7)                                                   00769800
 1090 FORMAT( 10X, 26HNO OF RUNS UP AND DOWN   =,I5/10X,26HEXPECTED NO O00769900
     1F RUNS      = ,F7.1/10X,26HS.D. OF NO OF RUNS       =,F8.2/10X    00770000
     2  26HMEAN SQ SUCCESSIVE DIFF  =,1PE16.7/10X,26HMEAN SQ SUCC DIFF/V00770100
     3AR    =,0PF9.3///10X,24HDEVIATIONS FROM WTD MEAN//15X,21HNO OF + S00770200
     4IGNS       =,I5/15X,21HNO OF - SIGNS       =I5/15X,10HNO OF RUNS, 00770300
     5 10X,1H=,I5/15X,21HEXPECTED NO OF RUNS =,F7.1/15X,12HS.D. OF RUNS,00770400
     6 8X,1H=,F8.2/15X,21HDIFF./S.D. OF RUNS  =F9.3)                    00770500
 1100 FORMAT(/////68H NOTE - ITEMS IN PARENTHESES REFER TO PAGE NUMBER I00770600
     1N NBS HANDBOOK 91)                                                00770700
 1110 FORMAT(//   27X,12HOBSERVATIONS,47X,20HORDERED OBSERVATIONS)      00770800
 1120 FORMAT(1H0,8X,1HI,9X, 4HX(I),9X,4HRANK,7X,  9HX(I)-MEAN,7X,4HW(I) 00770900
     1, 16X, 3HNO.,8X,4HX(J), 10X, 11HX(J+1)-X(J))                      00771000
 1130 FORMAT(1H0,8X,1HI, 9X, 4HX(I),9X,4HRANK,7X,  9HX(I)-MEAN,27X,     00771100
     1  3HNO.,8X, 4HX(J), 10X, 11HX(J+1)-X(J))                          00771200
 1140 FORMAT(I10,1PE17.7,0PF9.1,1PE17.7,22X,I6,1P2E17.7)                00771300
 1150 FORMAT(I10,1PE17.7,0PF9.1,1PE17.7,1PE12.3,10X,I6,1P2E17.7)        00771400
 1160 FORMAT(1H )                                                       00771500
      END                                                               00771600
C  96  54      SUBROUTINE STMT( NSTMT )            2 19 68              00771700
      SUBROUTINE STMT( NSTMT )                                          00771800
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00771900
C                                                                       00772000
C     THIS SUBROUTINE ASSEMBLES AND CHECKS A STATEMENT NUMBER.          00772100
C                                                                       00772200
C     CALLED BY..  .MAIN.                                               00772300
C                                                                       00772400
      MISC=10*KARD(M)                                                   00772500
  10  M=M+1                                                             00772600
      K=KARD(M)                                                         00772700
      IF(K.GE.10)GO TO 30                                               00772800
      MISC=10*(MISC+K)                                                  00772900
      IF(MISC.LT.10000)GO TO 10                                         00773000
C                                                                       00773100
C     ILLEGAL STATEMENT NUMBER EXIT                                     00773200
C                                                                       00773300
  20  KARG=1                                                            00773400
      RETURN                                                            00773500
C                                                                       00773600
C     NON-NUMERIC FOUND, IS IT A .                                      00773700
C                                                                       00773800
  30  IF(K.EQ.37)GO TO 50                                               00773900
C                                                                       00774000
C     IS IT A /                                                         00774100
C                                                                       00774200
  40  IF(K.EQ.36)GO TO 70                                               00774300
C                                                                       00774400
C     IS IT A SPACE                                                     00774500
C                                                                       00774600
      IF(K-44)20,60,20                                                  00774700
C                                                                       00774800
C     . FOUND, MUST BE FOLLOWED BY ONE AND ONLY ONE NUMERAL             00774900
C                                                                       00775000
  50  M=M+1                                                             00775100
      K=KARD(M)                                                         00775200
      IF(K.GE.10)GO TO 20                                               00775300
      MISC=MISC+K                                                       00775400
  60  M=M+1                                                             00775500
      K=KARD(M)                                                         00775600
      GO TO 40                                                          00775700
  70  M=M+1                                                             00775800
      K=KARD(M)                                                         00775900
C                                                                       00776000
C     / FOUND, MUST BE FOLLOWED BY BLANKS THEN/OR A LETTER              00776100
C                                                                       00776200
      IF(K.EQ.44)GO TO 70                                               00776300
      IF(K.GE.36.OR.K.LT.10)GO TO 20                                    00776400
C                                                                       00776500
C     LEGAL STATEMENT NUMBER FOUND                                      00776600
C                                                                       00776700
      NSTMT=MISC                                                        00776800
      KARG=0                                                            00776900
      RETURN                                                            00777000
      END                                                               00777100
C  97  78      SUBROUTINE STORE( J )               2 19 68              00777200
      SUBROUTINE STORE( J )                                             00777300
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00777400
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00777500
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00777600
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00777700
      DIMENSION ARGS(100)                                               00777800
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00777900
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00778000
C         STORAGE LAYOUT..          STATEMENT NUMBER                    00778100
C                                   NUMBER OF WORDS IN ENTRY            00778200
C                                   NARGS+64*(L1+64*L2)                 00778300
C     ALL ITEMS ARE STORED IN       (   ENTRY 1    )                    00778400
C     FLOATING POINT TO ALLOW       (         2    )                    00778500
C     CONVERSION TO DOUBLE-              .....                          00778600
C     PRECISION.                    ( LAST WORD    )                    00778700
C                                                                       00778800
      IF(IOVFL .NE. 0 ) RETURN                                          00778900
      IZE = J + 2                                                       00779000
      IF( NSTMT .GT. NSTMTH ) GO TO 80                                  00779100
C                                                                       00779200
C         STATEMENT IS AN INSERTION OR A REPLACEMENT                    00779300
C                                                                       00779400
      L = NSTMT                                                         00779500
      L = LOCATE( L )                                                   00779600
      IF( L .GT. 0 ) GO TO 30                                           00779700
C                                                                       00779800
      L = -L                                                            00779900
      IDIF = IZE                                                        00780000
   10 LL = NCOM                                                         00780100
C         STATEMENT IS AN INSERTION, OPEN GAP                           00780200
      II = LL + IDIF                                                    00780300
      IF( II .GE. LCOM ) GO TO 90                                       00780400
      DO 20 I = L, NCOM                                                 00780500
      COM( II ) = COM( LL )                                             00780600
      II = II - 1                                                       00780700
   20 LL = LL - 1                                                       00780800
      GO TO 60                                                          00780900
C                                                                       00781000
C         STATEMENT IS REPLACEMENT                                      00781100
C                                                                       00781200
   30 IDIF = IZE - IFIX( COM( L+1 ) )                                   00781300
      IF( IDIF ) 40, 60, 10                                             00781400
C                                                                       00781500
C         NEW STATEMENT SMALLER THAN OLD, CLOSE UP GAP.                 00781600
C                                                                       00781700
   40 I = L - IDIF                                                      00781800
      II = L                                                            00781900
      DO 50 JJ= I, NCOM                                                 00782000
      COM( II ) = COM( JJ)                                              00782100
   50 II = II + 1                                                       00782200
C                                                                       00782300
C         INSERT STATEMENT                                              00782400
C                                                                       00782500
   60 COM( L ) = NSTMT                                                  00782600
      COM( L+1 ) = IZE                                                  00782700
      COM( L+2 ) = NARGS + 64 * ( L1 + 64 * L2 )                        00782800
      NCOM = NCOM + IDIF                                                00782900
      IF( IZE .EQ. 3 ) GO TO 75                                         00783000
      DO 70 I = 4, IZE                                                  00783100
      COM( L+3 ) = ARGTAB( I-3 )                                        00783200
   70 L = L + 1                                                         00783300
   75 CONTINUE                                                          00783400
      RETURN                                                            00783500
C                                                                       00783600
C         PUT STATEMENT ON END                                          00783700
C                                                                       00783800
   80 L = NCOM                                                          00783900
      IDIF = IZE                                                        00784000
      NSTMTX = NSTMTH                                                   00784100
      NSTMTH = NSTMT                                                    00784200
      IF( NCOM + IDIF .LT. LCOM ) GO TO 60                              00784300
C                                                                       00784400
C         COM STORAGE OVERFLOW                                          00784500
C                                                                       00784600
   90 IOVFL = 1                                                         00784700
      CALL ERROR( 12 )                                                  00784800
      RETURN                                                            00784900
      END                                                               00785000
C  98  26      SUBROUTINE STRUVE                   2 19 68              00785100
      SUBROUTINE STRUVE (Z,A,B)                                         00785200
      DOUBLE PRECISION Z,A,B,C,X,P,Q,R,S                                00785300
      COMMON /RJN/C(100),X                                              00785400
      X=DABS(Z)                                                         00785500
      IF (X.GT.70.) GO TO 2                                             00785600
      CALL BEJN                                                         00785700
      P=.0D0                                                            00785800
      Q=.0D0                                                            00785900
      DO 1 N=1,49                                                       00786000
      J=2*N                                                             00786100
      K=J+1                                                             00786200
      R=J-1                                                             00786300
      S=4*N**2-1                                                        00786400
      P=P+C(J)/R                                                        00786500
    1 Q=Q+C(K)/S                                                        00786600
      A=P/.78539816339D0                                                00786700
      B=(2.D0*Q+1.D0-C(1))/1.5707963268D0                               00786800
      GO TO 3                                                           00786900
    2 S=1.D0/X**2                                                       00787000
      P=1.D0-S*(1.D0-9.D0*S*(1.D0-25.D0*S*(1.D0-49.D0*S)))              00787100
      A=DBEY(X,0)+P/(X*1.5707963268D0)                                  00787200
      Q=1.D0+S*(1.D0-3.D0*S*(1.D0-15.D0*S*(1.D0-35.D0*S)))              00787300
      B=DBEY(X,1)+Q/(  1.5707963268D0)                                  00787400
    3 RETURN                                                            00787500
      END                                                               00787600
C  99 172      SUBROUTINE TRANSF                   2 19 68              00787700
      SUBROUTINE TRANSF                                                 00787800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00787900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00788000
      DIMENSION ARGS(100)                                               00788100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00788200
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00788300
C *****                                                                 00788400
C     SUBROUTINE TO PROVIDE TRANSFORMATIONS  B=UAU(T) AND  C=U(I)AU     00788500
C     L2=1    TRANSFORMATION  B=UAU(T)                                  00788600
C             GENERAL FORMS OF  TRANSFORM                               00788700
C                TRANSFORM   A(,) R=N   R=M ROWS OF U (,) STORE IN C(,) 00788800
C                M(XAXT)     A(,) K,K   U(,)  N,K         STORE IN C(,) 00788900
C                M(XAXT)     A(,) K     U(,)  N,K         STORE IN C(,) 00789000
C     L2=2    BACK TRANSFORMATION  C=U(T)ALL                            00789100
C             GENERAL FORMS OF BACKTRANS                                00789200
C                BACKTRANS   A(,) R=N  R=M ROWS OF U(T)(,) STORE IN C(,)00789300
C                M(XTAX)     A(,) N,N   U(,)  N,K          STORE IN C(,)00789400
C                M(XTAX)     A(,) N     U(,)  N,K          STORE IN C(,)00789500
C *****                                                                 00789600
      COMMON / SCRAT / X,NS                                             00789700
      DIMENSION A(10000)                                                00789800
      DOUBLE PRECISION X(5000), SUM                                     00789900
      DIMENSION  IR(4),ISAVE(2)                                         00790000
      EQUIVALENCE (IR,ISAVE)                                            00790100
      COMMON /MULTC/NS2                                                 00790200
C *****                                                                 00790300
C     CHECK TO SEE IF WE HAVE CORRECT NUMBER OF ARGUMENTS               00790400
C *****                                                                 00790500
      IF(NARGS.GT.10.OR.NARGS.LT.8) CALL ERROR(10)                      00790600
C *****                                                                 00790700
C     CHECK TO SEE IF ALL ARGUMENTS ARE INTEGERS                        00790800
C *****                                                                 00790900
      J=NARGS                                                           00791000
      CALL CKIND(J)                                                     00791100
      IF(J.NE.0) CALL  ERROR(3)                                         00791200
C *****                                                                 00791300
C     CHECK TO SEE IF DIMENSIONS ARE CORRECT                            00791400
C *****                                                                 00791500
      IF(NARGS.EQ.8) GO TO 230                                          00791600
      IF(NARGS.EQ.10) GO TO (200,  220  ),L2                            00791700
      GO TO (160,180),L2                                                00791800
 160  IF(IARGS(3).NE.IARGS(7)) CALL ERROR(3)                            00791900
      GO TO 230                                                         00792000
 180  IF(IARGS(3).NE.IARGS(6)) CALL ERROR(3)                            00792100
      GO TO 230                                                         00792200
 200  IF(IARGS(3).NE.IARGS(4).OR.IARGS(3).NE.IARGS(8)) CALL ERROR(3)    00792300
      GO TO 230                                                         00792400
  220 IF(IARGS(3).NE.IARGS(4).OR.IARGS(3).NE.IARGS(7)) CALL ERROR(3)    00792500
C *****                                                                 00792600
C     CHECK TO SEE IF DIMENSIONS ARE OUT OF RANGE                       00792700
C *****                                                                 00792800
  230 IR(1)=IARGS(1)                                                    00792900
      IR(2)=IARGS(2)                                                    00793000
      IR(3)=IARGS(3)                                                    00793100
      IR(4)=IARGS(3)                                                    00793200
      CALL MACHK(IR,J)                                                  00793300
      IF(J.NE.0) CALL ERROR(17)                                         00793400
      IF(NARGS.EQ.9) GO TO 300                                          00793500
      IR(1)=IARGS(5)                                                    00793600
      IR(2)=IARGS(6)                                                    00793700
      IF(NARGS.EQ.10) GO TO 280                                         00793800
      GO TO (240 , 260 ),L2                                             00793900
 240  IR(3)=IARGS(4)                                                    00794000
      IR(4)=IARGS(3)                                                    00794100
      GO TO 340                                                         00794200
 260  IR(3)=IARGS(3)                                                    00794300
      IR(4)=IARGS(4)                                                    00794400
      GO TO 340                                                         00794500
 280  IR(3)=IARGS(7)                                                    00794600
      IR(4)=IARGS(8)                                                    00794700
      GO TO 340                                                         00794800
 300  DO 320    I=1,4                                                   00794900
 320  IR(I)=IARGS(I+3)                                                  00795000
 340  CALL MACHK(IR,J)                                                  00795100
      IF(J.NE.0) CALL ERROR(17)                                         00795200
      IR(1)=IARGS(NARGS-1)                                              00795300
      IR(2)=IARGS(NARGS)                                                00795400
      IF(NARGS.EQ.8) GO TO 420                                          00795500
      GO TO (360, 380),L2                                               00795600
 360  IJ=7                                                              00795700
      GO TO 400                                                         00795800
 380  IJ=8                                                              00795900
  400 IF(NARGS.EQ.9) GO TO 410                                          00796000
      IR(3)=IARGS(IJ)                                                   00796100
      IR(4)=IARGS(IJ)                                                   00796200
      GO TO 440                                                         00796300
  410 IR(3)=IARGS(IJ-1)                                                 00796400
      IR(4)=IARGS(IJ-1)                                                 00796500
      GO TO 440                                                         00796600
 420  IR(3)=IARGS(4)                                                    00796700
      IR(4)=IARGS(4)                                                    00796800
 440  CALL MACHK(IR,J)                                                  00796900
      IF(J.NE.0)  CALL ERROR(17)                                        00797000
C *****                                                                 00797100
C     CHECK FOR PREVIOUS ERRORS                                         00797200
C *****                                                                 00797300
      IF(NERROR.NE.0) RETURN                                            00797400
C *****                                                                 00797500
C     FIND ADDRESSES OF COLUMNS                                         00797600
C *****                                                                 00797700
      NP=NARGS                                                          00797800
      ISAVE(1)=IARGS(1)                                                 00797900
      ISAVE(2)=IARGS(3)                                                 00798000
      IARGS(1)=IARGS(2)                                                 00798100
      IF(NARGS.EQ.9) GO TO 460                                          00798200
      IARGS(2)=IARGS(6)                                                 00798300
      GO TO 480                                                         00798400
 460  IARGS(2)=IARGS(5)                                                 00798500
 480  IARGS(3)=IARGS(NARGS)                                             00798600
      NARGS=3                                                           00798700
      CALL CHKCOL(J)                                                    00798800
      IARGS(1)=IARGS(1)+ISAVE(1)-1                                      00798900
      IF(NP.EQ.9) GO TO 500                                             00799000
      IARGS(2)=IARGS(2)+IARGS(5)-1                                      00799100
      GO TO 520                                                         00799200
 500  IARGS(2)=IARGS(2)+IARGS(4)-1                                      00799300
 520  IARGS(3)=IARGS(3)+IARGS(NP-1)-1                                   00799400
      NS2=NS/2                                                          00799500
      IROWA=ISAVE(2)                                                    00799600
      ISP=1                                                             00799700
      IF(NP.EQ.8) GO TO 560                                             00799800
      IF(NP.EQ.10) GO TO 540                                            00799900
      IROWU=IARGS(L2+5)                                                 00800000
      GO TO 580                                                         00800100
 540  IROWU=IARGS(L2+6)                                                 00800200
      GO TO 580                                                         00800300
 560  IROWU=IARGS(4)                                                    00800400
 580  GO TO (600 ,620),L2                                               00800500
 600  IADD1=1                                                           00800600
      IADD2=NROW                                                        00800700
      GO TO 640                                                         00800800
 620  IADD1=NROW                                                        00800900
      IADD2=1                                                           00801000
 640  DO  720     J=1,IROWU                                             00801100
      DO  700     I=1,IROWU                                             00801200
      IUP=IARGS(2)+(I-1)*IADD1                                          00801300
      IA=IARGS(1)                                                       00801400
      IUT=IARGS(2)+(J-1)*IADD1                                          00801500
      ISX=NS2                                                           00801600
      DO   680    L=1,IROWA                                             00801700
      IU=IUP                                                            00801800
      DO   660    K=1,IROWA                                             00801900
      X(ISX)=RC(IU)*RC(IA)*RC(IUT)                                      00802000
      ISX=ISX-1                                                         00802100
      IU=IU+IADD2                                                       00802200
      IA=IA+1                                                           00802300
 660  CONTINUE                                                          00802400
      IA=IA+NROW-IROWA                                                  00802500
      IUT=IUT+IADD2                                                     00802600
 680  CONTINUE                                                          00802700
      CALL SORTSM (IROWA*IROWA,SUM)                                     00802800
      A(ISP)=SUM                                                        00802900
      ISP=ISP+1                                                         00803000
  700 CONTINUE                                                          00803100
      IC=IC+NROW-IROWU                                                  00803200
  720 CONTINUE                                                          00803300
C *****                                                                 00803400
C     STORE RESULTS IN WORKSHEET                                        00803500
C *****                                                                 00803600
      IS=1                                                              00803700
      IC=IARGS(3)                                                       00803800
      DO 820 J=1,IROWU                                                  00803900
      DO 800 I=1,IROWU                                                  00804000
      RC(IC)=A(IS)                                                      00804100
      IS=IS+1                                                           00804200
      IC=IC+1                                                           00804300
  800 CONTINUE                                                          00804400
      IC=IC+NROW-IROWU                                                  00804500
  820 CONTINUE                                                          00804600
      RETURN                                                            00804700
      END                                                               00804800
C 100  21      SUBROUTINE VARCON(NAME)             2 19 68              00804900
      SUBROUTINE VARCON(NAME)                                           00805000
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00805100
      DIMENSION NAME(2),N(14)                                           00805200
      DATA N(1),N(2),N(3),N(4),N(5),N(6),N(7),N(8),N(9),N(10),N(11),    00805300
     1 N(12),N(13),N(14)/10705,2604,16038,16767,17496,18225,18954,1377, 00805400
     2 15001,5*0/                                                       00805500
C                                                                       00805600
C      LOOKUP NAME IN VARIABLE-NAME TABLE                               00805700
C                                                                       00805800
C        NAMES IN TABLE                                                 00805900
C                                                                       00806000
C             NRMAX,COLTOP,V,W,X,Y,Z                                    00806100
C                                                                       00806200
      DO 10 IM=1,7                                                      00806300
      I = IM                                                            00806400
      IF(NAME(1).EQ.N(I).AND.NAME(2).EQ.N(I+7))GO TO 20                 00806500
  10  CONTINUE                                                          00806600
      I=0                                                               00806700
  20  ARG=I                                                             00806800
      RETURN                                                            00806900
      END                                                               00807000
C 101  14      SUBROUTINE VECTOR( A, J )           2 19 68              00807100
      SUBROUTINE VECTOR( A, J )                                         00807200
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00807300
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00807400
      DIMENSION ARGS(100)                                               00807500
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00807600
C                                                                       00807700
C        VECTORIZE A IN TO COLUMN STARTING AT J                         00807800
C                                                                       00807900
      IF( NRMAX .EQ. 0 ) GO TO 20                                       00808000
      K = J + NRMAX - 1                                                 00808100
      DO 10 I = J, K                                                    00808200
  10  RC( I ) = A                                                       00808300
  20  RETURN                                                            00808400
      END                                                               00808500
C 102   6      SUBROUTINE X(S)                     2 19 68              00808600
      SUBROUTINE X(S)                                                   00808700
      PRINT 10, S                                                       00808800
   10 FORMAT(A6)                                                        00808900
      RETURN                                                            00809000
      END                                                               00809100
C 103 346      SUBROUTINE XECUTE                   2 19 68              00809200
      SUBROUTINE XECUTE                                                 00809300
      COMMON / FLAGS / NSUMRY, LLIST                                    00809400
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00809500
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00809600
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00809700
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00809800
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00809900
      DIMENSION ARGS(100)                                               00810000
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00810100
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00810200
      COMMON / BLOCKX / INDEX( 6, 8 ), LEVEL                            00810300
   90 IF ( L1 .LE. 30 ) GO TO                                           00810400
     1(100, 200, 200, 400, 500, 200, 700, 800, 900, 1000, 1100,         00810500
     2 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200,00810600
     3 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000 ), L1             00810700
      CALL GOJOB                                                        00810800
      GO TO 9000                                                        00810900
  100 CALL RESET                                                        00811000
      GO TO 9000                                                        00811100
  200 CALL PRINTX                                                       00811200
      GO TO 9000                                                        00811300
  400 IF( L2 .NE. 1 ) GO TO 402                                         00811400
  401 NSUMRY = 0                                                        00811500
      GO TO 9000                                                        00811600
C                                                                       00811700
C      NO LIST   OR  NOLIST                                             00811800
C                                                                       00811900
C      LIST  (WITH NO ARGUMENT)  = LIST 3                               00812000
C      LIST 0 = NO LISTING          LIST 1 = LIST ONLY INFORMATIVE DIAGS00812100
C      LIST 2 = LIST ONLY ARITH ERR LIST 3 = LIST BOTH TYPES OF ERRORS  00812200
C                                                                       00812300
C      IF A FATAL ERROR OCCURS, LLIST IS SET TO AND KEPT AT 3           00812400
C                                                                       00812500
  402 IARGS(1) = 0                                                      00812600
  404 IF( NERROR .EQ. 0 ) LLIST = IARGS( 1 )                            00812700
      WRITE( ISCRAT, 406 ) IARGS( 1 )                                   00812800
  406 FORMAT(1H,,I1,82X)                                                00812900
      GO TO 9000                                                        00813000
C                                                                       00813100
C      LIST                                                             00813200
C                                                                       00813300
  410 IF( NARGS .EQ. 0 .OR. IARGS(1) .LT. 0 .OR. IARGS(1).GT.3)         00813400
     1 IARGS(1) = 3                                                     00813500
      GO TO 404                                                         00813600
  500 CALL READX                                                        00813700
      GO TO 9000                                                        00813800
  700 CALL DUMMY                                                        00813900
      GO TO 9000                                                        00814000
  800 CALL MXTX                                                         00814100
      GO TO 9000                                                        00814200
  900 CALL APRINT                                                       00814300
      GO TO 9000                                                        00814400
 1000 GO TO 9000                                                        00814500
 1100 CALL ARITH                                                        00814600
      GO TO 9000                                                        00814700
1200  CALL FUNCT                                                        00814800
      GO TO 9000                                                        00814900
 1300 GO TO ( 1301,1302,1303,1304,1305, 401,1307,1308,1309,1310,1312,   00815000
     1 1312,1313,1305), L2                                              00815100
 1301 CALL GENER                                                        00815200
      GO TO 9000                                                        00815300
 1302 CALL SET                                                          00815400
      GO TO 9000                                                        00815500
 1303 CONTINUE                                                          00815600
 1304 CALL FIXFLO                                                       00815700
      GO TO 9000                                                        00815800
 1305 CALL PLOT                                                         00815900
      GO TO 9000                                                        00816000
 1307 NSUMRY = 1                                                        00816100
      GO TO 9000                                                        00816200
 1308 CALL PAGE( 4 )                                                    00816300
      GO TO 9000                                                        00816400
 1309 CALL SPACE                                                        00816500
      GO TO 9000                                                        00816600
 1310 CALL PHYCON( 0 )                                                  00816700
      GO TO 9000                                                        00816800
 1312 CALL PHYCON( -1 )                                                 00816900
      GO TO 9000                                                        00817000
 1313 CALL PAGEX                                                        00817100
      GO TO 9000                                                        00817200
 1400 IF( L2 .GT. 2 ) GO TO 1410                                        00817300
      CALL BEGIN                                                        00817400
      GO TO 9000                                                        00817500
 1410 IF( L2 .LE. 5 ) GO TO 13000                                       00817600
      IF( L2 .LE. 8 ) GO TO 14000                                       00817700
      CALL IFS                                                          00817800
      GO TO 9000                                                        00817900
 1500 CALL MOP                                                          00818000
      GO TO 9000                                                        00818100
 1600 CALL INVERT                                                       00818200
      GO TO 9000                                                        00818300
 1700 IF( L2 .EQ. 2 ) GO TO 1720                                        00818400
      CALL MMULT                                                        00818500
      GO TO 9000                                                        00818600
 1720 CALL MRAISE                                                       00818700
      GO TO 9000                                                        00818800
 1800 CALL MATRIX                                                       00818900
      GO TO 9000                                                        00819000
 1900 CALL ALLSUB                                                       00819100
      GO TO 9000                                                        00819200
 2000 CALL MSCROW                                                       00819300
      GO TO 9000                                                        00819400
 2100 GO TO ( 2101, 2101, 2103, 2104, 2104, 2104, 2104, 2108, 2108,     00819500
     1 2110, 2111, 2112, 2113,2108,410 , 402, 9000), L2                 00819600
 2101 CALL PROROW                                                       00819700
      GO TO 9000                                                        00819800
 2103 CALL DEFINE                                                       00819900
      GO TO 9000                                                        00820000
 2104 CALL EXTREM                                                       00820100
      GO TO 9000                                                        00820200
 2108 CALL SORDER                                                       00820300
      GO TO 9000                                                        00820400
 2110 CALL ERASE                                                        00820500
      GO TO 9000                                                        00820600
 2111 CALL EXCHNG                                                       00820700
      GO TO 9000                                                        00820800
 2112 CALL FLIP                                                         00820900
      GO TO 9000                                                        00821000
 2113 CALL CHANGE                                                       00821100
      GO TO 9000                                                        00821200
 2200 CALL ORTHO                                                        00821300
      GO TO 9000                                                        00821400
2300  GO TO ( 2310, 2310, 2310, 2310, 2310, 2320, 2320,2320, 2320, 2330,00821500
     1 2330, 2340, 2350, 2350 ), L2                                     00821600
2310  CALL MISC2                                                        00821700
      GO TO 9000                                                        00821800
2320  CALL MOVE                                                         00821900
      GO TO 9000                                                        00822000
2330  CALL PDMOTE                                                       00822100
      GO TO 9000                                                        00822200
2340  CALL DIMENS                                                       00822300
      GO TO 9000                                                        00822400
2350  CALL SEPINS                                                       00822500
      GO TO 9000                                                        00822600
 2400 GO TO ( 2410, 2410, 2430, 2440, 2450 ), L2                        00822700
 2410 CALL STATIS                                                       00822800
      GO TO 9000                                                        00822900
 2430 CALL ERROR( 0 )                                                   00823000
      GO TO 9000                                                        00823100
 2440 CALL ERROR( -1 )                                                  00823200
      GO TO 9000                                                        00823300
 2450 CALL FPROB                                                        00823400
      GO TO 9000                                                        00823500
 2500 CALL SELECT                                                       00823600
      GO TO 9000                                                        00823700
 2600 CONTINUE                                                          00823800
      CALL YATES                                                        00823900
      GO TO 9000                                                        00824000
 2700 CONTINUE                                                          00824100
      CALL EXPCON                                                       00824200
 2800 CONTINUE                                                          00824300
 2900 CONTINUE                                                          00824400
 3000 CONTINUE                                                          00824500
 9000 CALL AERR( 0 )                                                    00824600
 9009 IF( LEVEL .GT. 0 ) GO TO 13130                                    00824700
      RETURN                                                            00824800
C         THIS IS WHERE THE REPEAT = EXECUTE = PERFORM COMMAND IS       00824900
C         EXECUTED.  NESTED PERFORMS UP TO EIGHT LEVELS ARE ALLOWED.    00825000
C         CURRENT LEVEL IS STORED IN  -LEVEL- .                         00825100
C                                                                       00825200
C     INDEX( 1, LEVEL ) CONTAINS LOCATION OF COMMAND AT ARG1 (FIRST)    00825300
C     INDEX( 2, LEVEL ) CONTAINS RUNNING INDEX FROM ARG 1 TO ARG 2      00825400
C     INDEX( 3, LEVEL ) CONTAINS LOCATION OF COMMAND AT ARG2 (LAST)     00825500
C     INDEX( 4, LEVEL ) CONTAINS THIRD ARG ( REPEAT COUNT )             00825600
C     INDEX( 5, LEVEL ) CONTAINS CURRENT LEVEL COUNTER (1 TO ARG 3)     00825700
C     INDEX( 6, LEVEL ) CONTAINS STATEMENT NUMBER OF STATEMENT CURRENTLY00825800
C                  BEING EXECUTED.                                      00825900
C                                                                       00826000
13000 IF( NARGS - 3 ) 13002, 13010, 13008                               00826100
13002 IF( NARGS - 1 ) 13008, 13004, 13006                               00826200
C                                                                       00826300
C     SECOND ARG MISSING, MAKE SAME AS FIRST ARG                        00826400
C                                                                       00826500
13004 IARGS( 2 ) = IARGS( 1 )                                           00826600
      KIND( 2 ) = KIND( 1 )                                             00826700
C                                                                       00826800
C     THIRD ARG MISSING, SET TO INTEGER 1                               00826900
C                                                                       00827000
13006 IARGS( 3 ) = 1                                                    00827100
      KIND( 3 ) = 0                                                     00827200
      GO TO 13020                                                       00827300
13008 CALL ERROR( 10 )                                                  00827400
      GO TO 9000                                                        00827500
13010 IF( KIND( 3 ) .EQ. 0 .AND. IARGS( 3 ) .GT. 0 ) GO TO 13020        00827600
13015 CALL ERROR( 3 )                                                   00827700
      GO TO 9000                                                        00827800
13020 DO 13040 I = 1, 2                                                 00827900
      IF( KIND( I ) .EQ. 0 ) GO TO 13030                                00828000
      IARGS( I ) = 10. * ARGS( I ) + .5                                 00828100
      GO TO 13035                                                       00828200
13030 IARGS( I ) = 10 * IARGS( I )                                      00828300
13035 IF( IARGS( I ) .GT. NSTMTH ) GO TO 13038                          00828400
      IARGS( I ) = LOCATE( IARGS( I ))                                  00828500
      IF( IARGS( I ) .GT. 0 ) GO TO 13040                               00828600
13038 CALL ERROR ( 13 )                                                 00828700
      GO TO 9000                                                        00828800
13040 CONTINUE                                                          00828900
13045 IF( LEVEL .LT. 8 ) GO TO 13050                                    00829000
      CALL ERROR( 19 )                                                  00829100
      GO TO 9000                                                        00829200
13050 IF( NERROR .NE. 0 ) GO TO 9000                                    00829300
      LEVEL = LEVEL + 1                                                 00829400
      INDEX( 1, LEVEL ) = IARGS( 1 )                                    00829500
      INDEX( 3, LEVEL ) = IARGS( 2 )                                    00829600
      INDEX( 4, LEVEL ) = IARGS( 3 )                                    00829700
      INDEX( 5, LEVEL ) = 0                                             00829800
C         OUTER LOOP                                                    00829900
13100 INDEX( 5, LEVEL ) = INDEX( 5, LEVEL ) + 1                         00830000
      IF( INDEX( 5, LEVEL ) .LE. INDEX( 4, LEVEL ) ) GO TO 13110        00830100
C         END OF OUTER LOOP, REDUCE LEVEL BY 1                          00830200
      LEVEL = LEVEL - 1                                                 00830300
      GO TO 9009                                                        00830400
C         INNER LOOP                                                    00830500
13110 INDEX( 2, LEVEL ) = INDEX( 1, LEVEL )                             00830600
13130 I2 = INDEX( 2, LEVEL )                                            00830700
      IF( I2 .GT. INDEX( 3, LEVEL ) ) GO TO 13100                       00830800
      INDEX( 6, LEVEL ) = COM( I2)                                      00830900
      K = COM( I2 + 1 )                                                 00831000
      INDEX( 2, LEVEL ) = INDEX( 2, LEVEL ) + K                         00831100
      L2 = COM( I2 + 2 )                                                00831200
      L1 = L2 / 64                                                      00831300
      NARGS = L2 - 64 * L1                                              00831400
      L2 = L1 / 64                                                      00831500
      L1 = L1 - 64 * L2                                                 00831600
      CALL EXPAND( K - 2, COM( I2 + 3 ) )                               00831700
      GO TO 90                                                          00831800
C                                                                       00831900
C     L2 = 6,7,8  INCREMENT, INDEX, RESTORE                             00832000
C                                                                       00832100
14000 IF( L2 - 7 ) 14010, 14500, 14020                                  00832200
C                                                                       00832300
C     INCREMENT, T = 1.    RESTORE, T = 0.                              00832400
C                                                                       00832500
14010 T = 1.                                                            00832600
      GO TO 14030                                                       00832700
14020 T = 0.                                                            00832800
14030 IF( NARGS .GE. 2 ) GO TO 14040                                    00832900
14035 K = 10                                                            00833000
      GO TO 14410                                                       00833100
14040 IF( KIND( 1 ) .EQ. 0 ) GO TO 14050                                00833200
      J = 10. * ARGS( 1 ) + .5                                          00833300
      GO TO 14053                                                       00833400
14050 J = 10 * IARGS ( 1 )                                              00833500
14053 IF( J .GT. NSTMTH ) GO TO 14056                                   00833600
      J = LOCATE( J )                                                   00833700
C     J HAS LOCATION OF COMMAND TO BE MODIFIED                          00833800
      IF( J .GT. 0 ) GO TO 14060                                        00833900
14056 K = 13                                                            00834000
      GO TO 14410                                                       00834100
14060 JJ = J + IFIX( COM( J+1 ) )                                       00834200
C                                                                       00834300
C     CHECK THAT COMMAND HAS THE PROPER NUMBER OF ARGUMENTS             00834400
C                                                                       00834500
      IF( NARGS - 1 .NE. MOD( IFIX( COM( J+2) ), 64 ))GO TO 14035       00834600
      J = J + 3                                                         00834700
C     SKIP OVER HEADER                                                  00834800
C                                                                       00834900
C     CHECK IF THIS COMMAND IS STORED. IF SO, PULL OUT INTO ARGTAB.     00835000
C                                                                       00835100
      IF (LEVEL .EQ. 0 ) GO TO 14100                                    00835200
      K = 2 * NARGS                                                     00835300
      DO 14070 I = 2, K                                                 00835400
      ARGTAB( I ) = COM( I2+4 )                                         00835500
14070 I2 = I2 + 1                                                       00835600
C     I2 HAS LOCATION OF THIS COMMAND                                   00835700
14100 I = 2 + KIND( 1 )                                                 00835800
14200 IF( COM ( J ) ) 14280, 14210, 14260                               00835900
C                                                                       00836000
C     FLOATING POINT CONST.                                             00836100
C                                                                       00836200
14210 IF( ARGTAB( I ) ) 14212, 14220, 14400                             00836300
C     INCR. FLT. PT. CONSTANT BY "STATEMENT"                            00836400
14212 IF( ARGTAB( I ) .EQ. -1. ) GO TO 14400                            00836500
      CALL XPND( ARGTAB(I) , K , Y , KND )                              00836600
      IF( K .LT. 0 ) GO TO 14264                                        00836700
      IF( KND .EQ. 0 ) GO TO 14400                                      00836800
      COM( J+1 ) = T * COM( J+1 ) + Y                                   00836900
14214 J = J + 2                                                         00837000
14216 I = I + K + 1                                                     00837100
      GO TO 14250                                                       00837200
14220 COM( J+1 ) = T * COM( J+1 ) + ARGTAB( I+1 )                       00837300
14230 J = J + 2                                                         00837400
14240 I = I + 2                                                         00837500
14250 IF( J - JJ ) 14200, 14420, 14420                                  00837600
C                                                                       00837700
C     COLUMN NUMBER                                                     00837800
C                                                                       00837900
14260 IF ( ARGTAB( I ) ) 14262,14400,14268                              00838000
C                                                                       00838100
C     INTEGER CONSTANT MODIFIED BY "" STATEMANT""                       00838200
14262 IF ( ARGTAB( I ) .EQ. -1. ) GO TO 14400                           00838300
      CALL XPND( ARGTAB( I ) , K , Y,KND)                               00838400
      IF ( K .GE. 0 ) IF ( KND ) 14400,14266,14400                      00838500
14264 K = -K                                                            00838600
      GO TO 14410                                                       00838700
14266 COM( J ) = T * COM( J ) + Y                                       00838800
      J=J+1                                                             00838900
      GO TO 14216                                                       00839000
14268 COM( J ) = T * ( COM( J ) - 8192. ) + ARGTAB( I )                 00839100
      IF( COM( J ) ) 14390, 14390, 14270                                00839200
14270 J = J + 1                                                         00839300
      I = I + 1                                                         00839400
      GO TO 14250                                                       00839500
C                                                                       00839600
C     VARIABLE *REFERENCE*                                              00839700
C                                                                       00839800
C     NRMAX, V, W, X, Y, Z CAN ONLY INCREMENT BY 0 OR 0.                00839900
C     WHETHER 0 OR 0. INCREMENTS ""X"" OR"X" IS IMMATERIAL              00840000
C                                                                       00840100
14280 IF (COM( J ) .LT. -16. ) GO TO 14290                              00840200
      IF ( COM( J ) .EQ. -1. ) GO TO 14430                              00840300
C                                                                       00840400
C                                                                       00840500
      IF ( ABS( ARGTAB(  I ) ) + ABS( ARGTAB( I+1 ) ) .NE. 0. )         00840600
     1       IF ( ARGTAB( I ) - 8192. ) 14400,14270,14400               00840700
      J = J + 1                                                         00840800
      GO TO 14240                                                       00840900
C                                                                       00841000
C     *ROW,COL*REFERENCE.                                               00841100
C                                                                       00841200
14290 IF( ARGTAB( I ) + 16. ) 14295, 14400, 14400                       00841300
14295 COM( J ) = T * ( COM( J ) + 8208. ) + ARGTAB( I )                 00841400
      IF( COM( J ) .GT. -16. ) GO TO 14400                              00841500
      IF( COM( J+1 ) * ARGTAB( I+1 ) ) 14400, 14400, 14300              00841600
14300 Y = T * ( ABS( COM( J+1 ) ) - 8192. ) + ABS( ARGTAB( I+1 ) )      00841700
      IF ( Y ) 14400, 14400, 14310                                      00841800
14310 COM( J+1 ) = SIGN( Y, COM( J+1 ) )                                00841900
      GO TO 14230                                                       00842000
14390 K = 18                                                            00842100
      GO TO 14410                                                       00842200
14400 K = 20                                                            00842300
14410 CALL ERROR( K )                                                   00842400
14420 GO TO 9000                                                        00842500
C                                                                       00842600
C     *** (=THRU) IGNORE. INCREM. OR RESTORE MAY OR MAY NOT             00842700
C     HAVE CORRESPONDING ***                                            00842800
C                                                                       00842900
14430 IF ( ARGTAB( I ) .EQ. -1. ) I = I + 1                             00843000
      J = J + 1                                                         00843100
      GO TO 14250                                                       00843200
C                                                                       00843300
C     INDEX                                                             00843400
C                                                                       00843500
14500 CALL X( "INDEX" )                                                 00843600
      GO TO 9000                                                        00843700
      END                                                               00843800
C 104  25      SUBROUTINE XFORMT                   2 19 68              00843900
      SUBROUTINE XFORMT                                                 00844000
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00844100
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00844200
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00844300
C                                                                       00844400
C     LOOK FOR LETTER A-F FOLLOWED BY NON-ALPHANUMERIC CHARACTER        00844500
C     A $ = 46 STOPS THE SCAN                                           00844600
C                                                                       00844700
  10  M = M + 1                                                         00844800
      IF( KARD(M).LT.10 .OR. KARD(M).GT.15 ) IF(KARD(M)-46) 10,90,10    00844900
      IF( KARD( M+1 ) .LE. 35 ) GO TO 90                                00845000
      L2 = KARD( M )                                                    00845100
C                                                                       00845200
C     LOOK FOR (                                                        00845300
C                                                                       00845400
  20  M = M + 1                                                         00845500
      IF( KARD(M) .NE. 41 ) IF( KARD(M)-46 ) 20, 90, 20                 00845600
C                                                                       00845700
C     PACK UP FORMAT TO END OF CARD ($)                                 00845800
C                                                                       00845900
      CALL PK5500(KRDEND+3-M , NEWCD(M-2),IFMT( 1, L2-9 ))              00846000
  30  RETURN                                                            00846100
  90  CALL ERROR( 205 )                                                 00846200
      GO TO 30                                                          00846300
      END                                                               00846400
C 105  22      SUBROUTINE XHEAD                    2 19 68              00846500
      SUBROUTINE XHEAD                                                  00846600
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00846700
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00846800
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00846900
      GO TO 50                                                          00847000
  40  M = M + 1                                                         00847100
  50  IF( KARD( M) .GE. 10 ) IF( KARD( M ) - 46 ) 40, 70 , 40           00847200
      CALL AARGS                                                        00847300
      I = ARG                                                           00847400
      IF( KARG .EQ. 0 .AND. I .GT. 0 .AND. I .LT. 50 ) GO TO 100        00847500
  70  CALL ERROR( 204 )                                                 00847600
  80  RETURN                                                            00847700
  90  M = M + 1                                                         00847800
 100  IF( KARD( M ) .NE. 36 ) IF( KARD( M ) - 46 ) 90, 70, 90           00847900
C                                                                       00848000
C     SLASH FOUND. PICK UP NEXT 12 CHARACTERS  IN  FORMAT A1 AND PACK   00848100
C     INTO FORMAT A6. THIS PORTION OF THIS IS NON-STANDARD AND WILL     00848200
C     HAVE TO BE REWRITTEN FOR MACHINES OTHER THAN B5500                00848300
C                                                                       00848400
      CALL PK5500(12,NEWCD(M-1),IHEAD(1,I))                             00848500
      RETURN                                                            00848600
      END                                                               00848700
C 106  47      SUBROUTINE XPND( T , K , Y , KND )  2 19 68              00848800
      SUBROUTINE XPND( T , K , Y , KND )                                00848900
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00849000
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00849100
      DIMENSION ARGS(100)                                               00849200
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00849300
      DIMENSION T( 2 )                                                  00849400
C                                                                       00849500
C     THIS SUBROUTINE TAKES A ""STATEMENT"" REFERENCE AS STORED         00849600
C     AND EXPANDS IT INTO THE PROPER ARGUMENT WITH CHECKING.            00849700
C                                                                       00849800
C     K IS RETURNED 0 IF ARG IN STATEMENT IS ONE WORD LONG              00849900
C     K IS RETURNED 1 IF ARG IN STATEMENT IS TWO WORDS LONG             00850000
C     K IS RETURNED -( ERROR NUMBER ) IF ERROR OCCURS.                  00850100
C                                                                       00850200
      IT = -T( 1 )                                                      00850300
      IF( IT .LT. 16 ) GO TO 60                                         00850400
C                                                                       00850500
C     ""ROW,COL"" ENTRY                                                 00850600
C                                                                       00850700
      IT =IT - 8208                                                     00850800
      IF( IT .GT. 0 .AND. IT .LE. NROW ) GO TO 41                       00850900
      K = -16                                                           00851000
      GO TO 44                                                          00851100
   41 IARGS( 100 ) = ABS( T(2) ) - 8192                                 00851200
      KIND( 100 ) = 0                                                   00851300
      CALL ADRESS( 100 , J )                                            00851400
      IF( J .NE. 0 ) GO TO 46                                           00851500
      K = -11                                                           00851600
   44 RETURN                                                            00851700
   46 J = J + IT                                                        00851800
      KND= 0                                                            00851900
      IF( T(2) .LT. 0 ) KND = 1                                         00852000
      Y = RC( J - 1 )                                                   00852100
      K=1                                                               00852200
      GO TO 44                                                          00852300
C                                                                       00852400
C     NRMAX , V , W , X , Y , Z , REFERENCE.                            00852500
C                                                                       00852600
   60 IU = IT / 2                                                       00852700
      KND = IT - 2 * IU                                                 00852800
      K = 0                                                             00852900
      IF( IU .LE. 1 ) GO TO 70                                          00853000
      Y = VWXYZ( IU-2 )                                                 00853100
      GO TO 44                                                          00853200
   70 Y= NRMAX                                                          00853300
      GO TO 44                                                          00853400
      END                                                               00853500
C 107  55      SUBROUTINE XSTOP                    2 19 68              00853600
      SUBROUTINE XSTOP                                                  00853700
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00853800
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00853900
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00854000
      DIMENSION ARGS(100)                                               00854100
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00854200
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00854300
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00854400
      COMMON / SCRAT / A(10000),NS                                      00854500
      DIMENSION ITEMP( 84 )                                             00854600
      EQUIVALENCE ( ITEMP( 1 ), A( 1 ) )                                00854700
      COMMON / FLAGS / NSUMRY, LLIST                                    00854800
      DATA IZ, IP / 1HZ, 1H+ /                                          00854900
      DATA N0, KOMMA / 1H0,1H,  /                                       00855000
C                                                                       00855100
C     THIS ROUTINE REWINDS THE SCRATCH UNIT AND PRINTS IT.              00855200
C                                                                       00855300
      REWIND ISCRAT                                                     00855400
       LLIST = 0                                                        00855500
      IF ( NERROR .EQ. 0 ) LLIST = 3                                    00855600
  30  CALL PAGE( 0 )                                                    00855700
      WRITE( IPRINT, 35 )                                               00855800
  35  FORMAT(//)                                                        00855900
      DO 80 J = 1, 50                                                   00856000
      READ( ISCRAT, 40 ) ITEMP                                          00856100
  40  FORMAT( 84A1 )                                                    00856200
      IF( ITEMP( 1 ) . EQ. IZ ) GO TO 100                               00856300
      IF( ITEMP( 1 ) . EQ.IP ) GO TO 60                                 00856400
      IF ( ITEMP( 1 ) .EQ. KOMMA ) GO TO 55                             00856500
      WRITE( IPRINT, 50 ) ITEMP                                         00856600
  50  FORMAT(20X,84A1)                                                  00856700
      GO TO 80                                                          00856800
   55 LLIST = 3                                                         00856900
      IF ( ITEMP( 2 ) .EQ. N0 . AND . NERROR .EQ. 0 ) LLIST = 0         00857000
      GO TO 80                                                          00857100
  60  WRITE( IPRINT, 70 ) ( ITEMP( I ), I = 2, 84 )                     00857200
  70  FORMAT(18X,3A1,3X,80A1)                                           00857300
  80  CONTINUE                                                          00857400
      GO TO 30                                                          00857500
 100  REWIND ISCRAT                                                     00857600
      IF( NERROR - 1 ) 110,130,150                                      00857700
 110  WRITE( IPRINT, 120 )                                              00857800
 120  FORMAT(///40X,32HCONGRATULATIONS, NO FATAL ERRORS)                00857900
      GO TO 200                                                         00858000
 130  WRITE( IPRINT, 140 )                                              00858100
 140  FORMAT(///40X,20HONLY ONE FATAL ERROR)                            00858200
      GO TO 200                                                         00858300
 150  WRITE( IPRINT, 160 ) NERROR                                       00858400
 160  FORMAT(///40X,I4,7H ERRORS)                                       00858500
  200 WRITE( IPRINT , 180 )                                             00858600
  180 FORMAT(///80X,"CSD OMNITAB VERSION OF AUG.16,1968"/1H1)           00858700
      KRDKNT = 0                                                        00858800
      LLIST = 3                                                         00858900
      RETURN                                                            00859000
      END                                                               00859100
C 108  53      SUBROUTINE YATES                    2 19 68              00859200
      SUBROUTINE YATES                                                  00859300
C                                                                       00859400
C     YATES ALGORITHM                                                   00859500
C                                                                       00859600
C     STATEMENT IS                                                      00859700
C                                                                       00859800
C        YATES ALGORITHM FOR ++ FACTORS,OBS IN COL ++,STORE CONTRASTS IN00859900
C                                                                       00860000
C                                                                       00860100
C     ALL 3 ARGUMENTS MUST BE INTEGERS                                  00860200
C                                                                       00860300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00860400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00860500
      COMMON / SCRAT / A(10000),NS                                      00860600
C                                                                       00860700
C     CHECK ARGS                                                        00860800
C                                                                       00860900
      J=0                                                               00861000
      IF ( NARGS .EQ. 3 ) GO TO 10                                      00861100
      CALL ERROR(10)                                                    00861200
      J=1                                                               00861300
   10 DO 20 K=1,3                                                       00861400
      IF ( KIND( K ) .NE. 0 ) GO TO 30                                  00861500
   20 CONTINUE                                                          00861600
      IF ( J .EQ. 1 ) RETURN                                            00861700
      GO TO 40                                                          00861800
   30 CALL ERROR(3)                                                     00861900
      RETURN                                                            00862000
   40 NFACT= IARGS(1)                                                   00862100
      CALL ADRESS( 2 , NYOBS )                                          00862200
      CALL ADRESS( 3 , NYCONT )                                         00862300
      IF ( 2**NFACT .GT. NROW ) GO TO 30                                00862400
      IF ( NYOBS .EQ. 0 .OR. NYCONT .EQ. 0 ) GO TO 30                   00862500
      LL=NFACT*2**NFACT                                                 00862600
      IF ( NERROR .NE. 0 ) RETURN                                       00862700
      DO 50 I=1,LL                                                      00862800
   50 A(I)=0.0                                                          00862900
      L=2**NFACT                                                        00863000
      NY=NYOBS-1                                                        00863100
      DO 55 I=1,L                                                       00863200
   55 A(I)=RC(NY+I)                                                     00863300
      NYATES=L / 2                                                      00863400
      DO 60 I1=1,NFACT                                                  00863500
      DO 60 I2=1,NYATES                                                 00863600
      A(I1*L+I2)=A((I1-1)*L+2*I2)+ A( (I1-1)*L+2*I2-1)                  00863700
      A(I1*L+NYATES+I2) = A( (I1-1)*L+2*I2) - A( (I1-1)*L+2*I2-1)       00863800
   60 CONTINUE                                                          00863900
      L1 = NFACT * L                                                    00864000
      DO 70 I=1,L                                                       00864100
      RC(NYCONT)= A(L1+I)                                               00864200
   70 NYCONT=NYCONT+1                                                   00864300
      RETURN                                                            00864400
      END                                                               00864500
C 109 208C      MAIN AND CROSS REFERENCE TABLE     2 19 68              00864600
C      MAIN AND CROSS REFERENCE TABLE                                   00864700
C                                                                       00864800
C                                                                       00864900
C     THIS IS A CROSS-REFERENCE TABLE SHOWING WUICH SUBPROGRAMS         00865000
C     REFERENCE PARTICULAR BLOCKS OF COMMON OR PARTICULAR SUBPROGRAMS.  00865100
C     THIS LIST DOES NOT INCLUDE THOSE MANY SUBROUTINES CALLED ONLY BY  00865200
C     THE SUBROUTINE "XECUTE".                                          00865300
C                                                                       00865400
C     OMNITAB USES NO UNLABELLED COMMON.                                00865500
C                                                                       00865600
C                                                                       00865700
C                                                                       00865800
C                                                                       00865900
C******************** LABELLED COMMON **********************************00866000
C                                                                       00866100
C                                                                       00866200
C     ABCDEF                                                            00866300
C            BLOCK  OMCONV                                              00866400
C     BLOCKA                                                            00866500
C            AARGS  ASTER  BEGIN  INPUT  NNAME  NONBLA OMNIT  OUTPUT    00866600
C            PHYCON READX  SET    SETUP  STMT   STORE  VALUES XFORMT    00866700
C            XECUTE XHEAD  XOMNIT                                       00866800
C     BLOCKB                                                            00866900
C            BEGIN  INPUT  LOCATE OMNIT  OUTPUT STORE  XOMNIT XECUTE    00867000
C     BLOCKC                                                            00867100
C            AERR   ERROR  INPUT  INVERT OMNIT  OUTPUT RNDOWN SETUP     00867200
C            XOMNIT XSTOP  XECUTE                                       00867300
C     BLOCKD                                                            00867400
C            ADRESS ALLSUB APRINT ARITH  BEGIN  CHANGE CHKCOL CKIND     00867500
C            DEFINE DIMENS ERASE  ERROR  EXCHNG EXPAND EXPCON EXTREM    00867600
C            FIXFLO FLIP   FPROB  FUNCT  GENER  HEADS  IFS    INVERT    00867700
C            MACHK  MATRIX MISC2  MMULT  MOP    MOVE   MRAISE MSCROW    00867800
C            MTXCHK MXTX   OMNIT  ORTHO  OUTPUT PDMOTE PLOT   PRINTX    00867900
C            PROROW READQ  READX  RESET  SELECT SET    SETQ   SETUP     00868000
C            SORDER SPACE  STATIS STORE  TRANSF VECTOR XOMNIT XPND      00868100
C            XECUTE XSTOP                                               00868200
C     BLOCKE                                                            00868300
C            ALLSUB APRINT ARITH  BEGIN  EXPAND EXPCON EXTREM FIXFLO    00868400
C            FPROB  FUNCT  IFS    INVERT LOOKUP MATRIX MISC2  MMULT     00868500
C            MOP    MRAISE MSCROW MXTX   OMNIT  ORTHO  PDMOTE PRINTX    00868600
C            PROROW READX  RESET  SELECT SET    SETFMT SORDER STATIS    00868700
C            STORE  TRANSF XECUTE                                       00868800
C     BLOCKX                                                            00868900
C            AERR   ERROR  IFS    RNDOWN SETUP  XECUTE                  00869000
C     CONSTS                                                            00869100
C            AARGS  FUNCT  FSIN   FCOS   FEXP   SETUP  VALUES           00869200
C     FLAGS                                                             00869300
C            AERR   ERROR  OUTPUT XOMNIT XSTOP  XECUTE                  00869400
C     HEADER                                                            00869500
C            FIXFLO HEADS  OMNIT  PAGE   PLOT   PRINTX SETFMT SETUP     00869600
C            SPACE  VALUES XFORMT XHEAD  XOMNIT XSTOP                   00869700
C     MULTC                                                             00869800
C            MMULT  MRAISE MXTX   SORTSM TRANSF                         00869900
C     PCONST                                                            00870000
C            BLOCK  PHYCON SETUP                                        00870100
C     QRS                                                               00870200
C            READQ  READX  SET    SETQ                                  00870300
C     SCRAT                                                             00870400
C            EXPCON FPROB  INVERT MATRIX MISC2  MMULT  MOP    MRAISE    00870500
C            MXTX   ORTHO  PLOT   PROROW SELECT SETUP  SORDER SORTSM    00870600
C            STATIS TRANSF XSTOP                                        00870700
C     SPRV                                                              00870800
C            ERROR  SETUP  XOMNIT                                       00870900
C                                                                       00871000
C                                                                       00871100
C                                                                       00871200
C                                                                       00871300
C******************** SUBROUTINES AND FUNCTIONS ************************00871400
C                                                                       00871500
C                                                                       00871600
C     AARGS                                                             00871700
C            ASTER  OMNIT  XHEAD                                        00871800
C     ADRESS                                                            00871900
C            ALLSUB ARITH  CHANGE CHKCOL DEFINE EXCHNG EXPCON FPROB     00872000
C            FUNCT  GENER  IFS    INVERT MISC2  MOP    MOVE   MSCROW    00872100
C            ORTHO  SELECT SET    XPND                                  00872200
C     AERR                                                              00872300
C            ERROR  OMNIT  SETUP  XECUTE                                00872400
C     ASTER                                                             00872500
C            OMNIT                                                      00872600
C     CHKCOL                                                            00872700
C            ERASE  EXTREM FLIP   MISC2  PDMOTE PLOT   PRINTX PROROW    00872800
C            READX  SELECT SORDER STATIS                                00872900
C     CKIND                                                             00873000
C            APRINT EXPCON INVERT MATRIX MISC2  MMULT  MOP    MRAISE    00873100
C            MXTX   STATIS TRANSF                                       00873200
C     ERROR                                                             00873300
C            AARGS  ALLSUB APRINT ARITH  ASTER  BEGIN  CHANGE DEFINE    00873400
C            DIMENS ERASE  EXCHNG EXPAND EXPCON EXTREM FIXFLO FLIP      00873500
C            FPROB  FUNCT  FSIN   FCOS   FSQRT  FEXP   FLOG   GENER     00873600
C            IFS    INVERT MATRIX MISC2  MMULT  MOP    MOVE   MRAISE    00873700
C            MSCROW MXTX   OMNIT  ORTHO  PDMOTE PAGE   PRINTX PROROW    00873800
C            READQ  READX  RESET  SELECT SET    SETQ   SORDER   SPACE   00873900
C            STATIS STORE  TRANSF XFORMT XOMNIT XECUTE                  00874000
C     EXPAND                                                            00874100
C            OMNIT  XECUTE                                              00874200
C     FCOS                                                              00874300
C            FUNCT                                                      00874400
C     FEXP                                                              00874500
C            FUNCT  FEXP2                                               00874600
C     FEXP2                                                             00874700
C            ARITH  MATRIX MISC2                                        00874800
C     FEXP3                                                             00874900
C            MATRIX                                                     00875000
C     FLOG                                                              00875100
C            FUNCT  FEXP2                                               00875200
C     FLOG10                                                            00875300
C            FUNCT                                                      00875400
C     FSIN                                                              00875500
C            FUNCT                                                      00875600
C     FSQRT                                                             00875700
C            FUNCT  INVCHK MSCROW ORTHO  STATIS                         00875800
C     HEADS                                                             00875900
C            PRINTX                                                     00876000
C     INPUT                                                             00876100
C            OMNIT                                                      00876200
C     INVCHK                                                            00876300
C            INVERT                                                     00876400
C     LOCATE                                                            00876500
C            STORE  XECUTE                                              00876600
C     LOOKUP                                                            00876700
C            OMNIT                                                      00876800
C     MACHK                                                             00876900
C            INVERT                                                     00877000
C     MOVE                                                              00877100
C            MISC2  XECUTE                                              00877200
C     MTXCHK                                                            00877300
C            APRINT EXPCON MATRIX MMULT  MOP    MRAISE MXTX   TRANSF    00877400
C     NNAME                                                             00877500
C            ASTER  OMNIT                                               00877600
C     NONBLA                                                            00877700
C            ASTER                                                      00877800
C     OMCONV                                                            00877900
C            INPUT                                                      00878000
C     OMNIT                                                             00878100
C            OMNSYM OMNREL OMNITA MNITAB                                00878200
C     OUTPUT                                                            00878300
C            OMNIT                                                      00878400
C     PAGE                                                              00878500
C            ORTHO  PLOT   PRINTX STATIS XSTOP  XECUTE                  00878600
C     PGSIZE                                                            00878700
C            OMNIT                                                      00878800
C     PHYCON                                                            00878900
C            ASTER  XECUTE                                              00879000
C    PK5500                                                             00879100
C           XFORMT XHEAD HEADS                                          00879200
C     PRINTX                                                            00879300
C            APRINT XECUTE                                              00879400
C     PROB                                                              00879500
C            FPROB  STATIS                                              00879600
C     READQ                                                             00879700
C            OMNIT                                                      00879800
C     RNDOWN                                                            00879900
C            AERR   ERROR                                               00880000
C     SETFMT                                                            00880100
C            PRINTX                                                     00880200
C     SETQ                                                              00880300
C            OMNIT                                                      00880400
C     SETUP                                                             00880500
C            OMNIT                                                      00880600
C     SORTSM                                                            00880700
C            MMULT  MRAISE MXTX   TRANSF                                00880800
C     SPINV                                                             00880900
C            INVCHK                                                     00881000
C     STMT                                                              00881100
C            OMNIT                                                      00881200
C     STORE                                                             00881300
C            OMNIT                                                      00881400
C     TRANSF                                                            00881500
C            MXTX                                                       00881600
C     VARCON                                                            00881700
C            ASTER                                                      00881800
C     VECTOR                                                            00881900
C            DEFINE ERASE  EXTREM FUNCT  MISC2  MSCROW PDMOTE           00882000
C     XECUTE                                                            00882100
C            OMNIT                                                      00882200
C     XFORMT                                                            00882300
C            OMNIT                                                      00882400
C     XHEAD                                                             00882500
C            OMNIT                                                      00882600
C     XOMNIT                                                            00882700
C            OMNIT                                                      00882800
C     XPND                                                              00882900
C            EXPAND XECUTE                                              00883000
C     XSTOP                                                             00883100
C            OMNIT  XOMNIT                                              00883200
C                                                                       00883300
C******************** SYSTEM FUNCTIONS *********************************00883400
C                                                                       00883500
C     *ALOG                                                             00883600
C            FUNCT  FLOG                                                00883700
C     *ALOG10                                                           00883800
C            FUNCT                                                      00883900
C     *ATAN                                                             00884000
C            FUNCT                                                      00884100
C     *COS                                                              00884200
C            FCOS                                                       00884300
C     *EXP                                                              00884400
C            FEXP                                                       00884500
C     *SIN                                                              00884600
C            FSIN                                                       00884700
C     *SQRT                                                             00884800
C            FUNCT  FSQRT  ORTHO                                        00884900
C     *TANH                                                             00885000
C            FUNCT                                                      00885100
      CALL OMNIT                                                        00885200
      STOP                                                              00885300
      END                                                               00885400
C 110  60      SUBROUTINE XOMNIT(LG)               2 19 68              00885500
      SUBROUTINE XOMNIT(LG)                                             00885600
      COMMON / BLOCKF / NCTOP                                           00885700
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00885800
      COMMON /BLOCKB/NSTMT,NSTMTX,NSTMTH,NCOM,LCOM,IOVFL,COM(2000)      00885900
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00886000
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00886100
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00886200
      DIMENSION ARGS(100)                                               00886300
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00886400
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00886500
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00886600
      COMMON / FLAGS / NSUMRY, LLIST                                    00886700
      COMMON / SPRV / NERCON,NERR                                       00886800
      DATA LBLANK/1H /                                                  00886900
      DIMENSION IH( 4 )                                                 00887000
      DATA IH(1),IH(2),IH(3),IH(4) / 1H(, 4H1P8E, 3H15., 2H6) /         00887100
C                                                                       00887200
C     IF LG IS NEG, FIRST CARD WAS NOT "OMNITAB" CARD. IF LG= 0, FIRST  00887300
C     CARD = "OMNITAB", ELSE SUBSEQUENT "OMNITAB" CARD FOUND.           00887400
C                                                                       00887500
      IF(LG)300,200,100                                                 00887600
C                                                                       00887700
C     GO THROUGH "STOP" SEQUENCE AND RETURN                             00887800
 100  CALL XSTOP                                                        00887900
 200  DO 210 I=1,72                                                     00888000
 210  NMCARD(I)=NEWCD(I)                                                00888100
C                                                                       00888200
C     INITIALIZE SYSTEM                                                 00888300
C                                                                       00888400
 300  DO 310 I = 1, 64                                                  00888500
      DO 310 J = 1, 6                                                   00888600
 310  ITLE( I, J ) = LBLANK                                             00888700
      DO 315 I = 1, 50                                                  00888800
      IHEAD( 1, I ) = 0                                                 00888900
      IHEAD( 3 , I) = 0                                                 00889000
      IHEAD( 4 , I) = 0                                                 00889100
 315  IHEAD( 2, I ) = 0                                                 00889200
      DO 320 J = 1, 6                                                   00889300
 320  IFMT( 1, J ) = 0                                                  00889400
      DO 325 I = 1, 4                                                   00889500
 325  IFMTX( I ) = IH( I )                                              00889600
      MODE=1                                                            00889700
      NRMAX=0                                                           00889800
      NROW=101                                                          00889900
      NCOL= 99                                                          00890000
      NCTOP = 1                                                         00890100
      NERR = 0                                                          00890200
      LLIST = 3                                                         00890300
      NSUMRY = 0                                                        00890400
      NERROR = 0                                                        00890500
      NSTMT=0                                                           00890600
      NSTMTH=0                                                          00890700
      NCOM=1                                                            00890800
      CALL RANDM(0)                                                     00890900
      LCOM=2000                                                         00891000
      IOVFL=0                                                           00891100
      NPAGE = 0                                                         00891200
      DO 330 I=1,10100                                                  00891300
 330  RC(I)=0.                                                          00891400
      RETURN                                                            00891500
      END                                                               00891600
C 111  33      SUBROUTINE SETUP                    2 19 68              00891700
      SUBROUTINE SETUP                                                  00891800
      COMMON / BLOCKA/MODE,M,KARD(83),KARG,ARG,ARG2,NEWCD(80),KRDEND    00891900
      COMMON / SPRV / NERCON,NERR                                       00892000
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00892100
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00892200
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00892300
      DIMENSION ARGS(100)                                               00892400
      EQUIVALENCE( ARGS(1), RC(10001) )                                 00892500
      COMMON / SCRAT / A(10000),NS                                      00892600
      COMMON / HEADER/NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),     00892700
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00892800
      COMMON / BLOCKX / INDEX( 6, 8 ), LEVEL                            00892900
      COMMON / PCONST / P( 40 ), N( 40 )                                00893000
      COMMON/CONSTS/PI,E,HALFPI,DEG,RAD,XEXP,XTRIG,XALOG,CC( 192 )      00893100
      NERCON = 100                                                      00893200
      P( 1 ) = PI                                                       00893300
      P( 2 ) = PI                                                       00893400
      P( 3 ) = E                                                        00893500
      P( 4 ) = E                                                        00893600
      KRDKNT = 0                                                        00893700
      KRDEND = 80                                                       00893800
      NERROR = 0                                                        00893900
      LEVEL = 0                                                         00894000
      MODE=1                                                            00894100
      INUNIT=1                                                          00894200
      IPRINT=3                                                          00894300
      IPUNCH=2                                                          00894400
      ISCRAT=4                                                          00894500
      NS = 10000                                                        00894600
      KIO = 0                                                           00894700
      CALL AERR(-1)                                                     00894800
      RETURN                                                            00894900
      END                                                               00895000
C 112  27      SUBROUTINE NEWJOB                   2 19 68              00895100
      SUBROUTINE NEWJOB                                                 00895200
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00895300
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00895400
      DIMENSION ARGS(100)                                               00895500
      EQUIVALENCE( ARGS(1), RC(10101) )                                 00895600
      COMMON / BLOCKE / NAME( 4 ), L1, L2, ISR                          00895700
      DIMENSION K( 4 )                                                  00895800
C                  AD, DA, AV, V                                        00895900
      DATA K / 837, 2943, 1323, 16038 /                                 00896000
C              CHECK  AURANDOM OR ANRANDOM                              00896100
      IF (NAME(1).NE. 1314 .OR. NAME(2) .NE. 1111) GO TO 10             00896200
      L1 = 40                                                           00896300
      L2=1                                                              00896400
      GO TO 2                                                           00896500
   10 IF (NAME(1).NE. 1125 .OR. NAME(2) .NE. 1111) GO TO 40             00896600
      L1=40                                                             00896700
      L2=2                                                              00896800
   2  RETURN                                                            00896900
C                  L1 = 41 MDAMAD                                       00897000
C                  L1 = 42  ARYVEC                                      00897100
  40  IF( NAME(1) .NE. 9477 ) GO TO 200                                 00897200
      DO 50 I = 1, 4                                                    00897300
  50  IF( NAME( 3 ) .EQ. K( I ) ) GO TO 70                              00897400
      GO TO 2                                                           00897500
  70  L1 = I/3 + 41                                                     00897600
      L2 = 2 - MOD( I, 2 )                                              00897700
      GO TO 2                                                           00897800
C                  MKRON                                                00897900
  200 IF( NAME(1) .NE. 9792 .OR. NAME(2) .NE. 11313 ) GO TO 90          00898000
      L1 = 43                                                           00898100
      GO TO 2                                                           00898200
C     DUMP SCRATCH AREA                                                 00898300
   90 IF( NAME(1) .NE. 3496 .OR. NAME(2) .NE. 11664 ) GO TO 100         00898400
      L1 = 45                                                           00898500
      GO TO 2                                                           00898600
  100 CONTINUE                                                          00898700
      GO TO 2                                                           00898800
      END                                                               00898900
C 113   8      SUBROUTINE GOJOB                    2 19 68              00899000
      SUBROUTINE GOJOB                                                  00899100
      COMMON / BLOCKE / NAME(4), L1, L2, ISR                            00899200
      IF (L1.EQ.40)CALL RANDM(1)                                        00899300
      IF( L1 .EQ. 41 ) CALL MDAMAD                                      00899400
      IF( L1 .EQ. 42 ) CALL ARYVEC                                      00899500
      IF( L1 .EQ. 43 ) CALL MKRON                                       00899600
      IF( L1 .EQ. 45 ) CALL XDUMP                                       00899700
      RETURN                                                            00899800
      END                                                               00899900
C  114 52  SUBROUTINE AERR(I)                                           00900000
      SUBROUTINE AERR(I)                                                00900100
      COMMON /BLOCKC/ KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                   00900200
      COMMON /BLOCKX/ INDEX(6,8),LEVEL                                  00900300
      COMMON /FLAGS/ NSUMRY,LLIST                                       00900400
C*****                                                                  00900500
C WHEN ARITHMETIC TROUBLES DEVELOP, THIS ROUTINE TALLIES THEM AND THEN  00900600
C PRINTS THE RESULT WHEN THE COMMAND IS DONE                            00900700
C*****                                                                  00900800
      DIMENSION MESS(6)                                                 00900900
      IF(I)80,60,50                                                     00901000
C*****                                                                  00901100
C DATA COMING IN                                                        00901200
C*****                                                                  00901300
   50 J = MIN0(I,6)                                                     00901400
      MESS(J) = MESS(J) + 1                                             00901500
   55 RETURN                                                            00901600
C*****                                                                  00901700
C DUMP RESULTS, END OF COMMAND                                          00901800
C*****                                                                  00901900
   60 IF(LLIST.LT.2) GO TO 80                                           00902000
      DO 70 J = 1,6                                                     00902100
      IF(MESS(J).EQ.0) GO TO 70                                         00902200
      WRITE(ISCRAT,601)                                                 00902300
      WRITE(ISCRAT,291) MESS(J)                                         00902400
  291 FORMAT(34H** ARITHMETIC FAULT,ZERO RETURNED,I4,6H TIMES,40X)      00902500
      GO TO (201,202,203,204,205,206),J                                 00902600
  201 WRITE(ISCRAT,701)                                                 00902700
  701 FORMAT(35H** NEGATIVE ARGUMENT TO SQRT OR LOG,49X)                00902800
      GO TO 600                                                         00902900
  202 WRITE(ISCRAT,702)                                                 00903000
  702 FORMAT(43H** EVALUATION OF EXPONENT PRODUCES OVERFLOW,41X)        00903100
      GO TO 600                                                         00903200
  203 WRITE(ISCRAT,703)                                                 00903300
  703 FORMAT(45H** ARGUMENT OUT OF BOUNDS TO INVERSE FUNCTION,39X)      00903400
      GO TO 600                                                         00903500
  204 CONTINUE                                                          00903600
  205 CONTINUE                                                          00903700
  206 WRITE(ISCRAT,706) J                                               00903800
  706 FORMAT(16H** ERROR MESSAGE,I2,66X)                                00903900
  600 IF(LEVEL .NE. 0) CALL RNDOWN                                      00904000
      WRITE(ISCRAT,601)                                                 00904100
  601 FORMAT(84X)                                                       00904200
      MESS(J)=0                                                         00904300
   70 CONTINUE                                                          00904400
      GO TO 55                                                          00904500
C*****                                                                  00904600
C INITIALIZATION SECTION                                                00904700
C*****                                                                  00904800
   80 DO 85 J=1,6                                                       00904900
   85 MESS(J)=0                                                         00905000
      GO TO 55                                                          00905100
      END                                                               00905200
C  115  26 SUBROUTINE PK5500 (N,IALPH,JALPH)                            00905300
      SUBROUTINE PK5500 (N,IALPH,JALPH)                                 00905400
C*****                                                                  00905500
C     THIS ROUTINE PICKS UP CHARACTERS IN FORMAT A1 AND PACKS INTO      00905600
C     FORMAT A6. THIS ROUTINE IS NON-STANDARD AND WILL HAVE TO BE       00905700
C     REWRITTEN FOR MACHINES OTHER THAN BURROUGHS B-5500                00905800
C*****                                                                  00905900
      DIMENSION IALPH(1),JALPH(1,1)                                     00906000
      J=N/6                                                             00906100
      K=MOD(N,6)                                                        00906200
      L=1                                                               00906300
      DO 15 I=1,J                                                       00906400
      M=L+5                                                             00906500
      IS=12                                                             00906600
      DO 10 I1=L,M                                                      00906700
      JALPH(1,I)=CONCAT(JALPH(1,I),IALPH(I1),IS,12,6)                   00906800
   10 IS=IS+6                                                           00906900
   15 L=L+6                                                             00907000
      IF(K.EQ.0)RETURN                                                  00907100
      I=J+1                                                             00907200
      M=L+K                                                             00907300
      IS=12                                                             00907400
      DO 20 I1=L,M                                                      00907500
      JALPH(1,I)=CONCAT(JALPH(1,I),IALPH(I1),IS,12,6)                   00907600
   20 IS=IS+6                                                           00907700
      RETURN                                                            00907800
      END                                                               00907900
C 116    SUBROUTINE RANDM(IST)                                          00908000
      SUBROUTINE RANDM( IST )                                           00908100
C                                                                       00908200
C      THIS ROUTINE GENERATES RANDOM NUMBER. IT IS WRITTEN FOR          00908300
C      THE IBM 360/65. ( R L CHAMBERLAIN, JULY 1968 )                   00908400
C                                                                       00908500
C      THE COMMANDS ARE                                                 00908600
C   L2=1  (UNIFORM)                                                     00908700
C     AURANDOM  (,, ,++) ,,X,,                                          00908800
C     AURANDOM  (,, ,++) ,,X,,  STARTING WITH ,,                        00908900
C                                                                       00909000
C   L2=2                                                                00909100
C        (NORMAL)                                                       00909200
C     ANRANDOM  (,, ,++) ,,X,,                                          00909300
C     ANRANDOM  (,, ,++) ,,X,,  STARTING WITH                           00909400
C                                                                       00909500
C                                                                       00909600
C     REF.  MATH NOTE NO. 551, BOEING SCIENTIFIC RESEARCH LABS          00909700
C       MARSAGLIA AND BRAY, ONE-LINE RANDOM NUMBER GENERATORS           00909800
C                                                                       00909900
C                                                                       00910000
      COMMON / BLOCKF / NCTOP                                           00910100
      COMMON /BLOCKC/KIO,INUNIT,ISCRAT,KBDOUT,KRDKNT                    00910200
      DOUBLE PRECISION DEN                                              00910300
      COMMON /BLOCKD/ RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX, 00910400
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00910500
      COMMON/BLOCKE/NAME(4),L1,L2,ISRFLG                                00910600
      COMMON / RNDOM / INITX,LAMBDA                                     00910700
      IF ( IST .EQ. 0 ) GO TO 500                                       00910800
      L = 2**35-1                                                       00910900
      DEN=2**35                                                         00911000
      GO TO (10,10,30,40,50 ),NARGS                                     00911100
   10 K=9                                                               00911200
   15 CALL ERROR( K )                                                   00911300
      RETURN                                                            00911400
   30 IF ( KIND(1) + KIND(2) + KIND(3) .EQ. 0 ) GO TO 31                00911500
      K=3                                                               00911600
      GO TO 15                                                          00911700
   31 CONTINUE                                                          00911800
      IARGS(4)=IARGS(3)                                                 00911900
      KIND(4)=0                                                         00912000
      GO TO 60                                                          00912100
   40 IF ( KIND(1) + KIND(2) + KIND(3) + KIND(4) .EQ. 0 ) GO TO 41      00912200
      K=3                                                               00912300
      GO TO 15                                                          00912400
   41 CONTINUE                                                          00912500
      GO TO 60                                                          00912600
   50 IF ( KIND(1) + KIND(2) + KIND(3) + KIND(4) + KIND(5) .EQ. 0 )     00912700
     1 GO TO 51                                                         00912800
      K=3                                                               00912900
      GO TO 15                                                          00913000
   51 INITX = IARGS( 5 )                                                00913100
   60 IF ( (IARGS(4) + IARGS(2) - 1) .LE. NCOL ) GO TO 62               00913200
   61 K=16                                                              00913300
      GO TO 15                                                          00913400
   62 IF ( (IARGS(1) + IARGS(3) - 1) .GT.(NROW - NCTOP + 1) ) GO TO 61  00913500
      NNN=IARGS(4)                                                      00913600
      NNM=IARGS(3)                                                      00913700
      IF ( NERROR .NE. 0 ) RETURN                                       00913800
      DO 100 I=1,NNN                                                    00913900
      CALL ADRESS( 2, MMM )                                             00914000
      IF ( L2 .EQ. 1 ) GO TO 65                                         00914100
      IF (MOD(NNM,2) .NE. 0) NNM=NNM-1                                  00914200
   65 DO 90 I2=1,NNM                                                    00914300
      INITX = MOD( INITX*LAMBDA , L )                                   00914400
      RC( MMM + I2 - 1 ) = FLOAT( INITX ) / DEN                         00914500
      GO TO (90,70),L2                                                  00914600
   70 L1=1+MOD(I2,2)                                                    00914700
      GO TO (80,90),L1                                                  00914800
   80 J1=MMM+I2-1                                                       00914900
      J2=J1-1                                                           00915000
      XY=SQRT(-2.0*ALOG(RC(J2)) )                                       00915100
      RC(J2)= COS(6.283185*RC(J1))*XY                                   00915200
      RC(J1)= SIN(6.283185*RC(J1))*XY                                   00915300
   90 CONTINUE                                                          00915400
      IF ( L2 . EQ. 1 ) GO TO 100                                       00915500
      IF (MOD(IARGS(3),2) .EQ. 0) GO TO 100                             00915600
      INITX = MOD( INITX*LAMBDA , L )                                   00915700
      TEMP1 = FLOAT( INITX ) / DEN                                      00915800
      INITX = MOD( INITX*LAMBDA , L )                                   00915900
      TEMP2 = FLOAT( INITX ) / DEN                                      00916000
      XY = SQRT(-2.0*ALOG(TEMP1))                                       00916100
      TEMP1 = COS(6.283185*TEMP2)*XY                                    00916200
      MM1 = J1 + 1                                                      00916300
      RC(MM1) = TEMP1                                                   00916400
      NNM = NNM + 1                                                     00916500
  180 FORMAT(///20X,"SYSTEMS DESIGN AND PROGRAMMING OMNITAB VERSION OF A00916600
     1UG.16,1968"/20X,"NAVAL AIR TEST CENTER,PATUXENT RIVER,MD."/1H1)   00916700
  100 IARGS( 2 ) = IARGS( 2 ) + 1                                       00916800
      WRITE(ISCRAT,200) INITX                                           00916900
  200 FORMAT( 40H     **** THE LAST INTEGER GENERATED WAS,I12,5H ****,  00917000
     1 27X )                                                            00917100
      RETURN                                                            00917200
C                                                                       00917300
C     INITIALIZATION                                                    00917400
C                                                                       00917500
  500 CONTINUE                                                          00917600
      LAMBDA=5**13                                                      00917700
      INITX=LAMBDA                                                      00917800
      RETURN                                                            00917900
      END                                                               00918000
C 118  14   SUBROUTINE XDUMP                                            00918100
      SUBROUTINE XDUMP                                                  00918200
      COMMON/BLOCKD/RC(10100),IARGS(100),KIND(100),ARGTAB(100),NRMAX,   00918300
     1 NROW,NCOL,NARGS,VWXYZ(8),NERROR                                  00918400
      COMMON / HEADER /NMCARD(72),ITLE(64,6),IHEAD(4,50),IFMT(17,6),    00918500
     1 IFMTX(4),LNCNT,IPRINT,NPAGE,IPUNCH                               00918600
      IF(IARGS(1) .GT. 0 .AND. IARGS(1) .LE. 10000 ) GO TO 10           00918700
      IARGS(1) = 1000                                                   00918800
   10 CALL PAGE(0)                                                      00918900
      WRITE(IPRINT,100) IARGS(1)                                        00919000
  100 FORMAT(//10H THE FIRST,I10,30H LOCATIONS OF THE SCRATCH AREA/)    00919100
      CALL PNT( IARGS(1) )                                              00919200
      RETURN                                                            00919300
      END                                                               00919400
                                                                        00919500
                                                                        00919600
                                                                        99999999
