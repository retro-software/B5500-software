BEGIN                                                                   00000300
REAL WATE,DUMPS,Z,I,N,USED,AVAIL,SAVD,T,T1,S,LINKWD,NMIX;               00000400
 DEFINE JAR=31927#,      %    SYSTEM VIII-I                             00000500
 MIXMAX=12#;                                                            00000600
LABEL BRK;                                                              00000700
FORMAT FT(D"{!!!!!"I2":"I2":"I2"{!!~");                                 00000800
REAL STREAM PROCEDURE FETCH(A);                                         00000900
VALUE A;                                                                00001000
BEGIN    SI~A;     DI~LOC FETCH;       DS~WDS;        END;              00001100
STREAM PROCEDURE MOVE(S,CD,D);                                          00001200
VALUE CD;                                                               00001300
BEGIN    SI~S;          SI~SI+CD;                                       00001400
         DI~D;          DS~28 CHR;     DS~RESET;DS~5 SET;               00001500
END;                                                                    00001600
STREAM PROCEDURE STP(JAR,MIXMAX,C,STAB,TAB,NMIX);                       00001700
VALUE  JAR,MIXMAX;                                                      00001800
BEGIN    LOCAL POINT,MIX,NIMIX;                                         00001900
         LABEL OVER;                                                    00002000
OVER:    SI~JAR;        SI~SI+8;                                        00002100
         JAR~SI;        TALLY~MIX;                                      00002200
         TALLY~TALLY+1; MIX~TALLY;                                      00002300
         IF SB THEN                                                     00002400
         BEGIN          DI~LOC POINT;                                   00002500
         DS~WDS;        SI~POINT;                                       00002600
         DI~C;          SI~SI+1;                                        00002700
         DS~7 CHR;      DS~LIT "/";                                     00002800
         SI~SI+1;       DS~7 CHR;                                       00002900
         DS~LIT "=";    SI~LOC MIX;                                     00003000
         DS~2 DEC;      DS~LIT " ";                                     00003100
         SI~STAB;       DS~5 DEC;                                       00003200
         STAB~SI;       DS~LIT " ";                                     00003300
         SI~TAB;        DS~5 DEC;                                       00003400
         TAB~SI;        DS~2 LIT "{!";                                  00003500
         C~DI;                                                          00003600
         TALLY~NIMIX;   TALLY~TALLY+1;                                  00003700
         NIMIX~TALLY;                                                   00003800
         END            ELSE                                            00003900
         BEGIN          SI~STAB;                                        00004000
         SI~SI+8;       STAB~SI;                                        00004100
         SI~TAB;        SI~SI+8;                                        00004200
         TAB~SI;                                                        00004300
         END;                                                           00004400
         SI~LOC MIX;    DI~LOC MIXMAX;                                  00004500
         IF 8 SC ! DC THEN GO TO OVER;                                  00004600
         DI~C;                                                          00004700
         DS~4 LIT "{!!~";                                               00004800
         DI~NMIX;       SI~LOC NIMIX;                                   00004900
         DS~1 WDS;                                                      00005000
END STP;                                                                00005100
STREAM PROCEDURE STP2(STAB,TAB,USED,AVAIL,SAVD,NSAVD,LINKW,D);          00005200
VALUE                 STAB,TAB,USED,AVAIL,SAVD,NSAVD,LINKW   ;          00005300
BEGIN                                                                   00005400
DI~D;         SI~LOC STAB;                                              00005500
DI~DI+11;     DS~5 DEC;                                                 00005600
DI~DI+1;      DS~5 DEC;                                                 00005700
DI~DI+1;                                                                00005800
4  (     DI~DI+15;      DS~5 DEC;      ) ;                              00005900
DI~DI+3;      DS~3 DEC;                                                 00006000
END;                                                                    00006100
SAVE ARRAY STAB,TAB[0:MIXMAX];                                          00006200
ARRAY C[0:4|MIXMAX+1],D[0:16],W[0:4];                                   00006300
ALPHA FILE OUT DC 14 (2,5);                                             00006400
BEGIN                                                                   00006500
REAL STREAM PROCEDURE ADDRS(A);                                         00006600
BEGIN    SI~A;          ADDRS~SI;      END;                             00006700
STREAM PROCEDURE STP1(D);                                               00006800
BEGIN                                                                   00006900
DI~D;         DS~11 LIT "MCP USING  ";                                  00007000
DI~DI+5;      DS~LIT " ";                                               00007100
DI~DI+5;      DS~16 LIT "{!!IN-USE     = ";                             00007200
DI~DI+5;      DS~15 LIT "{!AVAILIABLE = ";                              00007300
DI~DI+5;      DS~15 LIT "{!SAVED      = ";                              00007400
DI~DI+5;      DS~15 LIT "{!NONSAVED   = ";                              00007500
DI~DI+5;      DS~3 LIT "{!!";                                           00007600
DI~DI+3;      DS~14 LIT " LINK WORDS{!~";                               00007700
END;                                                                    00007800
ARRAY A[0:1];                                                           00007900
IF STATUS(A[*]) > 0 THEN Z~A[0] ELSE GO TO BRK;                         00008000
IF WATE=0 THEN BEGIN DUMPS:=1;WATE:=5;END ELSE BEGIN                    00008100
 DUMPS:=WATE DIV 1000;WATE:=MAX(WATE MOD 1000,30);END;                  00008200
I~FETCH(ADDRS(A)-2).[33:15];                                            00008300
DO UNTIL I~FETCH(N~I).[33:15] = 0;                                      00008400
N~FETCH(N).[18:15];                                                     00008500
STP1(D[1]);                                                             00008600
W[0]~Z;                                                                 00008700
END;                                                                    00008800
WHILE DUMPS~DUMPS-1 } 0 DO                                              00008900
    BEGIN                                                               00009000
    USED~AVAIL~SAVD~LINKWD~0;                                           00009100
    FOR I~0 STEP 1 UNTIL MIXMAX DO TAB[I]~STAB[I]~0;                    00009200
    WHEN(WATE);                                                         00009300
    WRITE(DC(Z,90),FT,(I~TIME(1)/60)DIV 3600, I MOD 3600 DIV 60,        00009400
         I MOD 60)[BRK:BRK];                                            00009500
    I~FETCH(N).[18:15];                                                 00009600
    WHILE I!0 DO                                                        00009700
         BEGIN                                                          00009800
         I~(T~FETCH(T1~I)).[18:15];                                     00009900
         T1~T.[33:15]-T1-2;                                             00010000
         LINKWD~LINKWD+1;                                               00010100
         IF T { 0 THEN AVAIL~AVAIL +T1                                  00010200
                  ELSE                                                  00010300
              BEGIN                                                     00010400
              IF S~T.[9:6] > MIXMAX THEN                                00010500
                   BEGIN                                                00010600
                   FORMAT FD(O,"{!LOST AT CELL "                        00010700
                        5A1 "~");                                       00010800
  WRITE(DC(Z,90),FD,Z,T.[33:3],T.[36:3],T.[39:3],T.[42:3],              00010900
                        T.[45:3])[BRK:BRK];                             00011000
                   END;                                                 00011100
              USED~USED+T1;                                             00011200
 IF T.[3:6]<2 OR T.[3:6]>6 THEN S~0;                                    00011250
              IF BOOLEAN(T.[2:1]) THEN                                  00011300
                   BEGIN                                                00011400
                   SAVD~SAVD+T1;                                        00011500
                   STAB[S]~STAB[S]+T1;                                  00011600
                   END                                                  00011700
                                  ELSE                                  00011800
              IF T.[3:6] = 7 THEN TAB[0]~TAB[0]+T1                      00011900
                             ELSE TAB[S]~TAB[S]+T1;                     00012000
              END;                                                      00012100
         END;                                                           00012200
    STP(JAR,MIXMAX,C[1],STAB[1],TAB[1],NMIX);                           00012300
    STP2(STAB[0],TAB[0],USED,AVAIL,SAVD,USED-SAVD,LINKWD,D[1]);         00012400
    FOR I~8 STEP 28 UNTIL NMIX|32+11 DO                                 00012500
         BEGIN                                                          00012600
         MOVE(C[I DIV 8],ENTIER(I MOD 8),W[1]);                         00012700
         WRITE(DC(Z,90),5,W[*])[BRK:BRK];                               00012800
         END;                                                           00012900
    FOR I~8 STEP 28 UNTIL 131 DO                                        00013000
         BEGIN                                                          00013100
         MOVE(D[I DIV 8],ENTIER(I MOD 8),W[1]);                         00013200
         WRITE(DC(Z,90),5,W[*])[BRK:BRK];                               00013300
         END;                                                           00013400
    END;                                                                00013500
BRK:                                                                    00013600
END.                                                                    00013700
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
