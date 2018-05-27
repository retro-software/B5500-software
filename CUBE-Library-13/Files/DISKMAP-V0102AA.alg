DISKMAP     000102AA                                                    00000100
                                                                        00000200
                                                                        00000300
  **********************************************************************00000400
            PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES          00000500
      NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.     00000600
  **********************************************************************00000700
%                                                                       00000800
THIS PROGRAM SHOULD PRODUCE A MAP OF THE DISK BY AREA FROM THE          00000900
     DIRECTORY: IT MAY BE KLUDGED TO DO PDRCTRY1DISK FUNCTIONS ALSO.    00001000
%                                                                       00001100
BEGIN FILE IN DD DISK RANDOM "DIRCTRY" "DISK   " (1,30);                00001200
     FILE OUT P 1 (2,15);                                               00001300
    FORMAT ENTRY(A3,A4,"/",A3,A4," WAS CREATED ON ",O,", TO BE SAVED ", 00001400
         I3," DAYS, AND WAS LAST ACCESSED ON ",O/X8,"IT CONTAINS ",I6,  00001500
         " RECORDS, OF ",I4," WORDS, IN ",I4,"-WORD BLOCKS."/ I10,      00001600
         " ROWS WERE DECLARED, WITH ",I5," RECORDS (",I5," SEGMENTS) ", 00001700
         "PER ROW."//                                                   00001800
         *(X4,"ROW:    ",*I10/X4,"ADDRESS:  ",*I10//)///);              00001900
    ARRAY DAYS[0:11];                                                   00002000
    ALPHA PROCEDURE DATE(B); VALUE B; ALPHA B;                          00002100
    BEGIN INTEGER D,M;;                                                 00002200
         D~(B.[30:6]|10+B.[36:6])|10+B.[42:6];                          00002300
         DAYS[1]~REAL((B.[18:6]|10+B.[24:6])MOD 4=0)+28;                00002400
         FOR M~0 STEP 1 WHILE D>DAYS[M] DO D~D-DAYS[M];                 00002500
         D~D MOD 10 +(D DIV 10)|64;                                     00002600
         IF M~M+1>9 THEN M~M+54;                                        00002700
         DATE~"/00/00"&B[36:18:12]&D[18:36:12]&M[1:37:11];              00002800
    END DATE;                                                           00002900
    ARRAY EUS[0:9];                                                     00003000
     ARRAY NB,HDR[0:29];                                                00003100
     BOOLEAN PROCEDURE IP(A); ARRAY A[0];                               00003200
     BEGIN OWN INTEGER I,J,K;                                           00003300
          LABEL ROUND;                                                  00003400
         INTEGER L,M;;                                                  00003500
IF J=0 THEN J~-1;                                                       00003600
 ROUND:   IF K=0 OR K~K+1}30 THEN                                       00003700
          BEGIN DO IF I~I-1{0 THEN                                      00003800
               BEGIN READ(DD[J~J+16],30,NB[*]);                         00003900
                    I~15                                                00004000
               END UNTIL NB[2|I-2]!12;                                  00004100
               READ(DD[J+I-16],30,HDR[*]); % PRINT DIRECTORY HERE ******00004200
              IF NB[I|2-2]!76 THEN                                      00004300
              WRITE(P,ENTRY,NB[I|2-2].[6:18],NB[I|2-2],                 00004400
                            NB[I|2-1].[6:18],NB[I|2-1],                 00004500
                            DATE(HDR[3]),HDR[3].[1:17],                 00004600
                            DATE(HDR[4].[12:30]),                       00004700
                            HDR[7],HDR[0].[1:14],HDR[0].[15:15],        00004800
                            HDR[9],HDR[1],HDR[8],                       00004900
                            REAL(HDR[9]>10)+1, % FIRST *                00005000
                            FOR K~1 STEP 10 UNTIL HDR[9] DO             00005100
                            [L~IF HDR[9]-K>9  THEN 10 ELSE HDR[9]-K+1,  00005200
                             FOR M~1 STEP 1 UNTIL L DO M+K-1,L,         00005300
                             FOR M~1 STEP 1 UNTIL L DO HDR[M+K+8]]);    00005400
               K~10                                                     00005500
          END;                                                          00005600
          IF NB[2|I-2]=76 THEN IP~TRUE                                  00005700
                          ELSE                                          00005800
          IF A[0]~HDR[K]!0 THEN                                         00005900
          BEGIN A[1]~HDR[8];                                            00006000
               A[2]~NB[I|2-2];                                          00006100
               A[3]~NB[I|2-1];                                          00006200
               A[4]~K;                                                  00006300
          END ELSE GO ROUND;                                            00006400
     END IP;                                                            00006500
     PROCEDURE HV(A); ARRAY A[0]; A[0]~549755813887;                    00006600
     BOOLEAN PROCEDURE CMP(A,B); ARRAY A,B[0];                          00006700
          CMP~A[0]{B[0];                                                00006800
     INTEGER I;                                                         00006900
     FORMAT MCP(X7,"0 MCP RESIDENCE AND SCRATCH AREA (1003 SEGMENTS)."),00007000
         EUF(/"----- EU ",I1,22("-----")),                              00007100
         DRCTRY(X4,"1003 DISK DIRECTORY (",I5," SEGMENTS)."),           00007200
         OLAY  (I8," OVERLAY AREA (",I5," SEGMENTS)."),                 00007300
         ROW   (I8," ROW ",I2," OF ",A3,A4,"/",A3,A4," (",I5,           00007400
                   " SEGMENTS)."),                                      00007500
         AVAIL (I8," NOT ASSIGNED (",I6," SEGMENTS)."),                 00007600
         LAST (I8," TO END OF DISK IS NOT ASSIGNED (",I6,"SEGMENTS)."); 00007700
     PROCEDURE OP(B,A); VALUE B; BOOLEAN B; ARRAY A[0];                 00007800
     BEGIN OWN INTEGER LE;                                              00007900
         OWN INTEGER E,EU;                                              00008000
          IF B THEN                                                     00008100
         BEGIN WRITE(P,LAST,LE,E -LE);                                  00008200
               CLOSE(P);                                                00008300
          END ELSE                                                      00008400
          BEGIN IF LE=0 THEN                                            00008500
               BEGIN WRITE(P[1]);                                       00008600
                   E~EUS[0]|40000;                                      00008700
                    WRITE(P[DBL],MCP);                                  00008800
                    WRITE(P[DBL],DRCTRY,A[1]);                          00008900
                    LE~-1000-A[1];                                      00009000
               END ELSE                                                 00009100
              BEGIN WHILE A[0]!LE DO                                    00009200
                   IF LE<0 THEN WRITE(P[DBL],OLAY,ABS(LE),LE+LE~A[0])   00009300
                           ELSE                                         00009400
                   IF A[0]<E THEN WRITE(P[DBL],AVAIL,LE,-LE+LE~A[0])    00009500
                             ELSE                                       00009600
                   BEGIN IF LE!E THEN WRITE(P[DBL],AVAIL,LE,E-LE);      00009700
                        DO EU~EU+1 UNTIL EUS[EU]!0;                     00009800
                        WRITE(P[DBL],EUF,EU);                           00009900
                        E~EUS[EU]|40000+LE~EU|1000000                   00010000
                   END;                                                 00010100
                    WRITE(P[DBL],ROW,A[0],A[4]-9                        00010200
                                              ,FOR I~2,3 DO[A[I].[6:18],00010300
                                        A[I]],A[1]);                    00010400
                    LE~A[0]+A[1];                                       00010500
     END; END; END OF THE OUTPUT PROCEDURE;                             00010600
    EUS[0]~1;                                                           00010700
    COMMENT FILL EUS[|] WITH THE NUMBER OF DF STORAGE UNITS CONNECTED   00010800
         TO DF ELECTRONICS UNIT NR |;                                   00010900
    FILL DAYS[*] WITH 31,28,31,30,31,30,31,31,30,31,30,31;              00011000
     SORT(OP,IP,0,HV,CMP,5); END OF PROGRAM.                            00011100
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
