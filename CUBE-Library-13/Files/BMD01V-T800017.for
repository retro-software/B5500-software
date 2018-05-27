FILE  5 = CRA, UNIT = READER, RECORD = 10, BUFFER = 2                   00000100
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00000200
CID   0901HS 15 150    $BMD01V ANALYSIS OF VARIANCE FOR ONE-WAY DESIGN  00000300
CBMD01V  ANALYSIS OF VARIANCE FOR ONE-WAY DESIGN       MAY  4, 1965     00000400
      DIMENSION  DATE%2[, X%20000[, NC%5000[, FMT%120[, FT%12[,         00000500
     1 SDEV%12[,XBAR%12[,NUSE%20[,LCODE%72[,CONST%72[,L1%8[,D2%8[       00000600
      COMMON X,NCI,NCUE,    NTAPE,LUMP,LCODE,CONST,NH,FM,NTG,CASE       00000700
      EQUIVALENCE %SAMSIZ,ISAM[                                         00000800
C                                                                       00000900
  200 FORMAT%1H1, //                                                    00001000
     177H BMD01V - ANALYSIS OF VARIANCE  FOR ONE-WAY DESIGN - VERSION OF00001100
     2 MAY  4, 1965 /                                                   00001200
     341H HEALTH SCIENCES COMPUTING FACILITY, UCLA//[                   00001300
C                                                                       00001400
      YES # %&3HYES[                                                    00001500
      XNO # %&2HNO[                                                     00001600
      PROBLM # %&6HPROBLM[                                              00001700
      FINISH # %&6HFINISH[                                              00001800
      SAMSIZ # %&6HSAMSIZ[                                              00001900
      SPECTG#%&6HSPECTG[                                                00002000
      DO  666  I # 1, 20                                                00002100
  666 NUSE%I[ # 0                                                       00002200
      NEVER # 0                                                         00002300
      WRITE            ( 6, 200)                                        00002400
 1111 READ           (5,300)CODE,PROBNO,NP,OUT,NTG,BIN,WIND,NTAPE,IVF   00002500
      IF (CODE - FINISH) 1,1914,1                                       00002600
    1 IF (CODE-PROBLM)  607,2,607                                       00002700
    2 IF%%NP-1[*%NP-5001[[ 6001,6002,6002                               00002800
 6002 WRITE            (6,6102)                                         00002900
 6102 FORMAT%43HOERROR ON PROBLM CARD, CHECK PROGRAM LIMITS [           00003000
      CALL EXIT                                                         00003100
 6001 IF%NTG-9[ 6003,6003,6002                                          00003200
 6003 NCUE#1                                                            00003300
      IF (BIN-YES) 1113,1112,1113                                       00003400
 1112 NCUE#2                                                            00003500
 1113 IF ((BTWEEN(NP,2,5000))*(BTWEEN(IVF,0,10))*(BTWEEN(NCUE,1,2))*((BT00003600
     1WEEN(NTAPE,0,5))&(BTWEEN(NTAPE,7,15)))) 607,607,3                 00003700
    3 NUTS#NTAPE*%NTAPE-5[                                              00003800
      GO TO  %4, 5[, NCUE                                               00003900
    5 IF  %NUTS[  7, 607, 7                                             00004000
 4    CALL VFCHCK%IVF[                                                  00004100
    6 IF  %NUTS[  7, 8, 7                                               00004200
    8 NTAPE # 5                                                         00004300
      GO TO  9                                                          00004400
    7 NUSE%NTAPE[ # 1                                                   00004500
      IF (WIND-XNO) 667,9,667                                           00004600
  667 REWIND  NTAPE                                                     00004700
    9 IF  %NEVER[  10, 10, 11                                           00004800
   11 WRITE            ( 6, 200)                                        00004900
   10 NEVER # NEVER & 1                                                 00005000
      WRITE            ( 6, 400) PROBNO, NP                             00005100
      NCARDS#%NP&12[/13                                                 00005200
      I2#0                                                              00005300
      DO 17 J#1,NCARDS                                                  00005400
      I1#I2&1                                                           00005500
      I2#I2&13                                                          00005600
      IF%NP-I2[15,16,16                                                 00005700
   15 I2#NP                                                             00005800
   16 READ           (5,700)KODE,%NC%I[,I#I1,I2[                        00005900
      IF%KODE-ISAM[165,17,165                                           00006000
  165 NCARDS#-NCARDS                                                    00006100
   17 CONTINUE                                                          00006200
      IF%-NCARDS[18,18,6002                                             00006300
   18 CASE#0.0                                                          00006400
      DO 19 I#1,NP                                                      00006500
      IF%BTWEEN%NC%I[,1,20000[[607,607,181                              00006600
  181 CASE1#NC%I[                                                       00006700
      CASE#CASE&CASE1                                                   00006800
   19 CONTINUE                                                          00006900
      IF%CASE-%10.0**8[[195,195,607                                     00007000
  195 IF%NTG[607,499,401                                                00007100
  401 NH#0                                                              00007200
      DO 445 NI#1,NTG                                                   00007300
      READ           (5,425)FM,M,%L1%I[,D2%I[,I#1,M[                    00007400
      IF (FM-SPECTG) 403,404,403                                        00007500
  403 WRITE            (6,405)                                          00007600
      CALL EXIT                                                         00007700
  404 DO 6004  J#1,M                                                    00007800
      IF%L1%J[*%L1%J[-11[[ 6004,403,403                                 00007900
 6004 CONTINUE                                                          00008000
      DO 415 NG#1,M                                                     00008100
      NG1#NH&NG                                                         00008200
      LCODE%NG1[#L1%NG[                                                 00008300
  415 CONST%NG1[#D2%NG[                                                 00008400
  445 NH#NH&M                                                           00008500
  499 GO TO %12,13[,NCUE                                                00008600
   12 WRITE             (6, 500) IVF, NTAPE                             00008700
      IVF # IVF*12                                                      00008800
      READ           ( 5, 100) %FMT%I[, I # 1, IVF[                     00008900
      GO TO  14                                                         00009000
   13 READ           ( 5, 601) LUMP                                     00009100
      WRITE            ( 6, 600) LUMP, NTAPE                            00009200
   14 IF%NTG[607,439,426                                                00009300
  426 WRITE            (6,427)                                          00009400
      DO 428 NG#1,NH                                                    00009500
  428 WRITE            (6,431)LCODE%NG[,CONST%NG[                       00009600
  439 SPSQ#0.0                                                          00009700
      SMSQ # 0.0                                                        00009800
      GT # 0.0                                                          00009900
      NP1 # NP - 1                                                      00010000
      NCI # NC%1[                                                       00010100
      TOBS # NCI                                                        00010200
      CALL  READIN                                                      00010300
      DO  20  I # 1, NCI                                                00010400
   20 GT # GT & X%I[                                                    00010500
      Y # GT/TOBS                                                       00010600
      DO  21  J # 1, NCI                                                00010700
   21 SPSQ # SPSQ & %X%J[ - Y[**2                                       00010800
      NGO # 1                                                           00010900
      IF (OUT-YES) 30,31,30                                             00011000
   30 WRITE            ( 6, 801)                                        00011100
      GO TO  40                                                         00011200
   31 SDV # SQRT % SPSQ/%TOBS - 1.0[ [                                  00011300
      CALL  DECFND%Y, LY[                                               00011400
      CALL  DECFND%SDV, LSDV[                                           00011500
      IF  %NP - 12[  32, 32, 33                                         00011600
   33 WRITE            ( 6, 900)                                        00011700
      LY #  MAX0 %0, 7 - LY[                                            00011800
      LSDV #  MAX0 %0, 7 - LSDV[                                        00011900
      PUNCH  901, LY, LSDV                                              00012000
      READ  100, FT                                                     00012100
      WRITE            ( 6, FT) NGO, NCI, Y, SDV                        00012200
      NGO # 3                                                           00012300
      GO TO  40                                                         00012400
   32 LY #  MAX0 %LY, LSDV[                                             00012500
      LSDV #  MAX0 %0, 5 - LY[                                          00012600
      XBAR%1[ # Y                                                       00012700
      SDEV%1[ # SDV                                                     00012800
      NGO # 2                                                           00012900
   40 DO  50  I # 2, NP                                                 00013000
      NCI # NC%I[                                                       00013100
      XNI # NCI                                                         00013200
      TOBS # TOBS & XNI                                                 00013300
      CALL  READIN                                                      00013400
      AVE # 0.0                                                         00013500
      DO  51  J # 1, NCI                                                00013600
   51 AVE # AVE & X%J[                                                  00013700
      GT # GT & AVE                                                     00013800
      AVE # AVE/XNI                                                     00013900
      SDV # 0.0                                                         00014000
      DO  52  J # 1, NCI                                                00014100
   52 SDV # %X%J[ - AVE[**2 & SDV                                       00014200
      SPSQ # SPSQ & SDV                                                 00014300
      SMSQ # SMSQ & XNI*%AVE - Y[**2                                    00014400
      SDV # SQRT % SDV/%XNI - 1.0[ [                                    00014500
      GO TO  %50, 61, 62[, NGO                                          00014600
   61 XBAR%I[ # AVE                                                     00014700
      SDEV%I[ # SDV                                                     00014800
      GO TO  50                                                         00014900
   62 WRITE            ( 6, FT) I, NCI, AVE, SDV                        00015000
   50 CONTINUE                                                          00015100
      GO TO  %70, 71, 70[, NGO                                          00015200
   71 WRITE            ( 6, 1000) %I, I # 1, NP[                        00015300
 1000 FORMAT%////22H TREATMENT GROUP       , 12%I7, 2X[ [               00015400
      WRITE            ( 6, 1001) %NC%I[, I # 1, NP[                    00015500
      PUNCH  1002, LSDV                                                 00015600
      READ  100, FT                                                     00015700
      WRITE            ( 6, FT) %XBAR%I[, I # 1, NP[                    00015800
      PUNCH  1003, LSDV                                                 00015900
      READ  100, FT                                                     00016000
      WRITE            ( 6, FT) %SDEV%I[, I # 1, NP[                    00016100
   70 WRITE            ( 6, 1100 )                                      00016200
      GT # GT/TOBS                                                      00016300
      SMSQ # SMSQ - TOBS*%GT - Y[**2                                    00016400
      GT # SMSQ & SPSQ                                                  00016500
      TOBS # TOBS - 1.0                                                 00016600
      TLEVEL # NP1                                                      00016700
      DF # TOBS - TLEVEL                                                00016800
      AVE # SMSQ/TLEVEL                                                 00016900
      SDV # SPSQ/DF                                                     00017000
      FRATIO # AVE/SDV                                                  00017100
      WRITE            ( 6, 1200) SMSQ, NP1, AVE, FRATIO                00017200
      PUNCH  1300, DF, TOBS                                             00017300
      READ  1400, DF, SMSQ, TOBS, AVE                                   00017400
      WRITE            ( 6, 1500) SPSQ, DF, SMSQ, SDV                   00017500
      WRITE            ( 6, 1600) GT, TOBS, AVE                         00017600
      GO TO  1111                                                       00017700
  607 WRITE            ( 6, 6070 ) NP,IVF,NCUE,NTAPE                    00017800
      GO TO  1915                                                       00017900
 1914 DO  73  I # 1, 20                                                 00018000
      IF  %NUSE%I[[  72, 73, 72                                         00018100
   72 CALL  TAPEDO%I[                                                   00018200
   73 CONTINUE                                                          00018300
      WRITE            ( 6, 1900)                                       00018400
 1915 CALL EXIT                                                         00018500
 100  FORMAT%12A6[                                                      00018600
  300 FORMAT%2A6,I4,A3,I1,43X,A3,A2,2I2[                                00018700
  400 FORMAT%///// 15H PROBLEM CODE   , A6, /  20H NUMBER OF TREATMENT ,00018800
     1 9H GROUPS  , I4  [                                               00018900
  405 FORMAT%31H0ERROR ON TRANS-GENERATION CARD[                        00019000
  425 FORMAT%A6,I1,8%I2,F6.0[[                                          00019100
  427 FORMAT%1H023HSPECIAL TRANSGENERATION/1H 3X4HCODE4X8HCONSTANT[     00019200
  431 FORMAT%1H 5XI2,1XF11.5[                                           00019300
  500 FORMAT%34H NUMBER OF VARIABLE FORMAT CARDS   , I2  /              00019400
     1 18H DATA INPUT TAPE   , I2 [                                     00019500
  600 FORMAT%23H BINARY RECORD LENGTH  , I3 / 18H DATA INPUT TAPE  , I2[00019600
  601 FORMAT%I3[                                                        00019700
  700 FORMAT%A6, 1X, 13I5[                                              00019800
  801 FORMAT%////41H LISTING OF TREATMENT MEANS NOT REQUESTED   [       00019900
  900 FORMAT%//// 49H TREATMENT    SAMPLE SIZE      MEAN      STANDARD  00020000
     1 10H DEVIATION   //[                                              00020100
  901 FORMAT%17H%1X,I6,8X,I6,F16. , I1, 5H,F17. , I1, 1H[  [            00020200
 1001 FORMAT%/ 22H SAMPLE SIZE           , 12%I7, 2X[   [               00020300
 1002 FORMAT%34H%/22H MEAN                 , 12F9.  , I1, 1H[   [       00020400
 1003 FORMAT%34H%/22H STANDARD DEVIATION   , 12F9. , I1, 1H[   [        00020500
 1100 FORMAT%/////  33X, 21H ANALYSIS OF VARIANCE     ///               00020600
     1 19X, 15H SUM OF SQUARES, 6X, 2HDF, 5X, 12H MEAN SQUARE , 5X,     00020700
     2 8H F RATIO     /  [                                              00020800
 1200 FORMAT%15H0BETWEEN GROUPS      , F17.4, I10, 2F15.4  [            00020900
 1300 FORMAT%2F11.0[                                                    00021000
 1400 FORMAT%2A5, 1X, 2A5[                                              00021100
 1500 FORMAT%15H0WITHIN GROUPS    , F17.4, 2A5, F15.4[                  00021200
 1600 FORMAT%// 1H , 5X, 5HTOTAL, F21.4, 2A5[                           00021300
 1900 FORMAT%40H1COMPUTATION HALTED BY  FINISH  CARD.   [               00021400
 6070 FORMAT%1H1//49H THE CONTROL CARDS ARE INCORRECTLY PUNCHED AND/OR  00021500
     1 62H OUT OF ORDER, SO COMPUTATION CANNOT PROCEED.  SEE BMD MANUAL,00021600
     2 / 55H SECOND EDITION, FOR INSTRUCTIONS CONCERNING THE PROPER     00021700
     356H PREPARATION AND ORDERING OF CONTROL CARDS FOR BMD01V .   /    00021800
     4 5H NP = ,I4/, 6H IVF = ,I2/, 7H NCUE = ,I4/, 8H NTAPE = ,I2/  )  00021900
      STOP                                                              00022000
      END                                                               00022100
CBTWEEN    FUNCTION  BTWEEN%N, I, J[  #  1.0  IFF  N  BETWEEN  I  AND  J00022200
      FUNCTION  BTWEEN%N, I, J[                                         00022300
      IF  %N - I[  1, 2, 2                                              00022400
    2 IF  %J - N[  1, 3, 3                                              00022500
    3 BTWEEN # 1.0                                                      00022600
      RETURN                                                            00022700
    1 BTWEEN # 0.0                                                      00022800
      RETURN                                                            00022900
      END                                                               00023000
CDECFND   SUBROUTINE  DECFND%X, LEFT[ -  X  HAS MAGNITUDE  10**%LEFT-1[ 00023100
      SUBROUTINE  DECFND%X, LEFT[                                       00023200
      DIMENSION  A%20[                                                  00023300
      BLANK # %&1H [                                                    00023400
      PUNCH  10, X                                                      00023500
   10 FORMAT%F22.1[                                                     00023600
      READ  20, A                                                       00023700
   20 FORMAT%20A1[                                                      00023800
      DO  1  I # 1, 19                                                  00023900
      J # 21 - I                                                        00024000
      B # A%J[                                                          00024100
      IF (B-BLANK) 1,2,1                                                00024200
    1 CONTINUE                                                          00024300
    2 JJ # I - 1                                                        00024400
      IF  %JJ[  3, 3, 4                                                 00024500
    3 JJ # JJ & 1                                                       00024600
    4 LEFT # JJ                                                         00024700
      RETURN                                                            00024800
      END                                                               00024900
CDIVALG     DIVISION ALGORTHM                                           00025000
      SUBROUTINE  DIVALG%NA, NB, NQ, NR, NCODE[                         00025100
      NN # NA                                                           00025200
      IF  %NCODE[  1, 1, 2                                              00025300
    2 NN # NN - 1                                                       00025400
    1 NQ # NN/NB                                                        00025500
      NR # NA - NB*NQ                                                   00025600
      RETURN                                                            00025700
      END                                                               00025800
CREADIN     SUBROUTINE  READIN  OF  BMD01V     8-22-63                  00025900
      SUBROUTINE  READIN                                                00026000
      DIMENSION X%20000[,LCODE%72[,CONST%72[,FMT%120[                   00026100
      COMMON X,NCI,NCUE,    NTAPE,LUMP,LCODE,CONST,NH,FM,NTG            00026200
      GO TO  %1, 2[, NCUE                                               00026300
    1 READ           ( NTAPE, FMT) %X%I[, I # 1, NCI[                   00026400
    6 IF%NTG[7,8,7                                                      00026500
    7 CALL TRNGEN                                                       00026600
    8 RETURN                                                            00026700
    2 CALL  DIVALG%NCI, LUMP, NDO, LEFT, 1[                             00026800
      N2 # 0                                                            00026900
      IF  %NDO[  3, 3, 4                                                00027000
    4 DO  5  J # 1, NDO                                                 00027100
      N1 # N2 & 1                                                       00027200
      N2 # N2 & LUMP                                                    00027300
    5 READ      (NTAPE) %X%I[, I # N1, N2[                              00027400
    3 N1 # N2 & 1                                                       00027500
      READ      (NTAPE) %X%I[, I # N1, NCI[                             00027600
      GO TO 6                                                           00027700
      END                                                               00027800
CTAPEDO     SUBROUTINE  TAPEDO%LASTAP[   MANIPULATES TAPES FOR  BMD01V  00027900
      SUBROUTINE  TAPEDO%LASTAP[                                        00028000
      IF  %LASTAP - 5[  1, 2, 3                                         00028100
    1 REWIND  LASTAP                                                    00028200
      RETURN                                                            00028300
    3                                                                   00028400
    2 RETURN                                                            00028500
      END                                                               00028600
CTRNGEN       SUBROUTINE TRNGEN FOR BMD01V            JUNE 11, 1964     00028700
      SUBROUTINE TRNGEN                                                 00028800
      ASNF%X[#ATAN %X/SQRT %1.0-X**2[[                                  00028900
      DIMENSION X%20000[,LCODE%72[,CONST%72[,FMT%120[                   00029000
      COMMON X,NCI,NCUE,    NTAPE,LUMP,LCODE,CONST,NH,FM,NTG,SAMP       00029100
      DO 210 I#1,NH                                                     00029200
      FM#CONST%I[                                                       00029300
      JUMP#LCODE%I[                                                     00029400
      DO 8 J#1,NCI                                                      00029500
      D#X%J[                                                            00029600
      GO TO%10,20,30,40,50,60,70,80,90,110[,JUMP                        00029700
   10 IF%D[99,11,12                                                     00029800
   11 X%J   [#0.0                                                       00029900
      GO TO 8                                                           00030000
   12 X%J   [#SQRT %D[                                                  00030100
      GO TO 8                                                           00030200
   20 IF%D[99,21,22                                                     00030300
   21 X%J   [#1.0                                                       00030400
      GO TO 8                                                           00030500
   22 X%J   [#SQRT %D[&SQRT %D&1.0[                                     00030600
      GO TO 8                                                           00030700
   30 IF%D[99,99,31                                                     00030800
   31 X%J   [#ALOG10%D[                                                 00030900
      GO TO 8                                                           00031000
   40 X%J   [#EXP %D[                                                   00031100
      GO TO 8                                                           00031200
   50 IF%D[99,51,52                                                     00031300
   51 X%J   [#0.0                                                       00031400
      GO TO 8                                                           00031500
   52 IF%D-1.0[53,54,99                                                 00031600
   54 X%J   [#3.14159265/2.0                                            00031700
      GO TO 8                                                           00031800
   53 A#SQRT %D[                                                        00031900
      X%J   [#ASNF%A[                                                   00032000
      GO TO 8                                                           00032100
   60 A#D/%SAMP&1.0[                                                    00032200
      B#A&1.0/%SAMP&1.0[                                                00032300
      IF%A[99,61,62                                                     00032400
   61 IF%B[99,63,64                                                     00032500
   63 X%J   [#0.0                                                       00032600
      GO TO 8                                                           00032700
   64 X%J   [#ASNF%SQRT %B[[                                            00032800
      GO TO 8                                                           00032900
   62 IF%B[99,65,66                                                     00033000
   65 X%J   [#ASNF%SQRT %A[[                                            00033100
      GO TO 8                                                           00033200
   66 X%J   [#ASNF%SQRT %A[[&ASNF%SQRT %B[[                             00033300
      GO TO 8                                                           00033400
   70 IF%D[71,99,71                                                     00033500
   71 X%J   [#1.0/D                                                     00033600
      GO TO 8                                                           00033700
   80 X%J   [#D&FM                                                      00033800
      GO TO 8                                                           00033900
   90 X%J   [#D*FM                                                      00034000
      GO TO 8                                                           00034100
  110 IF%D[111,112,111                                                  00034200
  112 X%J   [#0.0                                                       00034300
      GO TO 8                                                           00034400
  111 X%J   [#D**FM                                                     00034500
    8 CONTINUE                                                          00034600
  210 CONTINUE                                                          00034700
      GO TO 1000                                                        00034800
   99 WRITE            (6,105)I,X%J[                                    00034900
      CALL EXIT                                                         00035000
  105 FORMAT%23H0TRANS-GENERATION ERROR//10H PASS NO.#I3,9H X VALUE#F10.00035100
     15[                                                                00035200
 1000 RETURN                                                            00035300
      END                                                               00035400
CVFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDS00035500
      SUBROUTINE VFCHCK%NVF[                                            00035600
      IF%NVF[10,10,20                                                   00035700
 10   WRITE            (6,4000 )                                        00035800
      NVF#1                                                             00035900
 50   RETURN                                                            00036000
C                                                                       00036100
 20   IF%NVF-10[50,50,10                                                00036200
C                                                                       00036300
 4000 FORMAT%1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00036400
     XIED, ASSUMED TO BE 1.[                                            00036500
      END                                                               00036600
