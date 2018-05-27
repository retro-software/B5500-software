         BEGIN                                                          00000100
COMMENT        CUBE LIBRARY NUMBER IS J400005.  THE PROGRAM NAME IS     00000200
               "TAPEDIR/LIST".  THIS VERSION DATED 5/8/68;              00000300
COMMENT       LIST AND PUNCH A LIBRARY TAPE DIRECTORY ;                 00000400
BOOLEAN       PUNCHTOG ;                                                00000500
FILE          LIBTAPE (1,200) ;                                         00000600
FILE OUT      LINE 4(2,15) ;                                            00000700
FILE OUT      PUNCH 0(2,10) ;                                           00000800
FILE OUT      SPO 11(1,12);                                             00000900
LABEL         CLOSEIT;                                                  00001000
REAL          MFI1,MFI2;                                                00001100
INTEGER       DATE;                                                     00001200
INTEGER       I,J,SEQ ;                                                 00001300
LABEL         EOF ;                                                     00001400
ARRAY         A[0:200] ;                                                00001500
LIST         LP(A[I].[6:6],A[I],A[I+1].[6:6],A[I+1],SEQ) ;              00001600
FORMAT        SPF ("WHEN #NO FILE LIBTAPE OCCURS USE  UL MESSAGE ",     00001700
                   "TAPEDIR/PUNCH  ~");                                 00001800
FORMAT FHEAD  (A1," LOAD FROM ",A1,A6," -"/                             00001900
               A1," DUMP TO   ",A1,A6," -"/                             00002000
                    A1," REMOVE -") ,                                   00002100
              FEND("LAZY/GUY ; END .") ,                                00002200
              FP(A1,A6,"/",A1,A6,",-",X55,I8) ,                         00002300
              FL(A1,A6,"/",A1,A6,",-",I9) ;                             00002400
              SEQ ~ 0 ;                                                 00002500
              WRITE (SPO,SPF);                                          00002600
              WHILE TRUE DO                                             00002700
              BEGIN                                                     00002800
                READ (LIBTAPE,10,A[*])[CLOSEIT];                        00002900
                MFI1 ~ A[1].[6:6];                                      00003000
                MFI2 ~ A[1];                                            00003100
              END;                                                      00003200
CLOSEIT:                                                                00003300
              CLOSE(LIBTAPE,*) ;                                        00003400
              WRITE (LINE[DBL],FHEAD,12,MFI1,MFI2,12,MFI1,MFI2,12);     00003500
              IF PUNCHTOG THEN                                          00003600
              WRITE (PUNCH,FHEAD,12,MFI1,MFI2,12,MFI1,MFI2,12);         00003700
      DO BEGIN                                                          00003800
              READ(LIBTAPE,*,FOR J ~ 0 STEP 1 UNTIL 199 DO A[J])[EOF] ; 00003900
              FOR I ~0 STEP 2 UNTIL J-2 DO                              00004000
         BEGIN                                                          00004100
              SEQ ~ SEQ + 1 ;                                           00004200
              WRITE(LINE[DBL],FL,LP) ;                                  00004300
              IF PUNCHTOG THEN                                          00004400
              WRITE(PUNCH,FP,LP) ;                                      00004500
         END ;                                                          00004600
         END UNTIL FALSE ;                                              00004700
EOF:          WRITE(LINE[DBL],FEND) ;                                   00004800
              IF PUNCHTOG THEN                                          00004900
              WRITE(PUNCH,FEND) ;                                       00005000
         END .                                                          00005100
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %% % % %  00005200
