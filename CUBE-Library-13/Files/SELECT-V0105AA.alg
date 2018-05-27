SELECT      000105AA                                                    00000100
                                                                        00000200
                                                                        00000300
************************************************************************00000400
           PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES           00000500
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000600
***********************************************************************;00000700
%THE FUNCTION OF THIS PROGRAM IS TO GIVE A COMPUTER                     00000800
%OPERATOR MORE FLEXIBILITY IN PRINTING A PRINTER                        00000900
%BACKUP TAPE (PBT).                                                     00001000
%                                                                       00001100
%THE FOLLOWING OPTIONS EXIST:                                           00001200
%   1) SEARCH FOR A PARTICULAR JOB ON THE PBT AND                       00001300
%      START PRINTING.                                                  00001400
%   2) SKIP UP THE PBT A CERTAIN NUMBER OF RECORDS. IF                  00001500
%      FIRST SKIP FACTOR IS NOT ENOUGH, THE OPERATOR                    00001600
%      MAY INSERT ANOTHER ONE. (HE MAY INSERT AS MANY                   00001700
%      SKIP FACTORS AS HE CHOOSES UNTIL AN END OF FILE                  00001800
%      CONDITION OCCURS.)                                               00001900
%                                                                       00002000
%EXAMPLES:                                                              00002100
%SUPPOSE THERE EXISTS A PBT ON MTD WITH THE FOLLOWING JOBS:             00002200
%   ALGOL/TEST                                                          00002300
%   TEST/LOADER                                                         00002400
%   COBOL/MIS023                                                        00002500
%   BLEST/B8500GR                                                       00002600
%   OCTDUMP/B8500GR                                                     00002700
%                                                                       00002800
%EXAMPLE 1 - FUNCTION IS TO SEARCH FOR COBOL/MIS023 AND                 00002900
%            START PRINTING FROM THERE.                                 00003000
%    SELECT/UTILITY = 2 BOJ                                             00003100
%    INPUT P = &/ORS =                                                  00003200
%    # SELECT/UTILITY = 2 ; ACCEPT                                      00003300
%    2 AXP=COBOL/MIS023                                                 00003400
%    # NO FIL PBT = 2                                                   00003500
%    2 IL MTD                                                           00003600
%                                                                       00003700
%EXAMPLE 2 - FUNCTION IS TO SKIP A CERTAIN AMOUNT OF                    00003800
%            RECORDS ON THE PBT. AFTER LOOKING AT THE                   00003900
%            PRINTOUT WE DECIDE TO SKIP SOME MORE.                      00004000
%    SELECT/UTILITY = 1 BOJ                                             00004100
%    INPUT P = &/ORS =                                                  00004200
%    # SELECT/UTILITY = 1 ; ACCEPT                                      00004300
%    1 AXS= 1000                                                        00004400
%    1 IL MTD                                                           00004500
%                                                                       00004600
%  AFTER SKIPPING 1000 RECORDS, PRINTING STARTS. SUPPOSE                00004700
%  THAT AFTER LOOKING AT OUTPUT WE DECIDE TO SKIP 2000 MORE             00004800
%  RECORDS. THEN THE FOLLOWING MESSAGE IS NECESSARY                     00004900
%    1 IN 25 = 2000                                                     00005000
%  YOU MAY USE THE ABOVE MESSAGE AS OFTEN AS NECESSARY.                 00005100
%                                                                       00005200
%EXAMPLE 3 - FUNCTION IS TO SEARCH FOR BLEST/B8500GR AND THEN           00005300
%            SKIP 500 RECORDS BEFORE PRINTING.                          00005400
%    SELECT/UTILITY = 5 BOJ                                             00005500
%    INPUT P = &/ORS =                                                  00005600
%    # SELECT/UTILITY = 5 ; ACCEPT                                      00005700
%    5 AXP= BLEST/B8500GR , S=500                                       00005800
%    # NO FIL PBT = 5                                                   00005900
%    5 IL MTD                                                           00006000
%                                                                       00006100
%EXAMPLE 4 - FUNCTION IS TO PRINT ENTIRE PBT. THIS                      00006200
%            EXAMPLE IS IDENTICAL TO THE FUNCTION OF THE                00006300
%            PRNPBT/DISK PROGRAM.                                       00006400
%    SELECT/UTILITY = 4 BOJ                                             00006500
%    INPUT P = &/ORS =                                                  00006600
%    # SELECT/UTILITY = 4 ; ACCEPT                                      00006700
%    4 AX                                                               00006800
%    # NO FIL PBT = 4                                                   00006900
%    4 IL MTD                                                           00007000
%                                                                       00007100
%                                                                       00007200
BEGIN                                                                   00007300
   INTEGER SKIPFACTOR;                                                  00007400
   FILE PBT (1, 90);                                                    00007500
   FILE SPO 11 (1, 10);                                                 00007600
   FILE LINE 1 (1, 15);                                                 00007700
   LABEL LT, L1, EXIT;                                                  00007800
   ARRAY Q, RECORD [0 : 18];                                            00007900
                                     % LOGICAL INPUT RECORD             00008000
   INTEGER OLDSKIP;                                                     00008100
   INTEGER RECORDCOUNT, LASTRECORDCOUNT;                                00008200
   REAL DSC;                                                            00008300
               % CONTROL FOR PRINTING                                   00008400
   INTEGER LOGREC;                                                      00008500
                       % LINE RECORD COUNTER  - 18 WDS                  00008600
   ARRAY PL[1: 8];                                                      00008700
                   % PARAMETER LIST                                     00008800
   REAL PLD;                                                            00008900
             % PBT PRINTER DESCRIPTOR IN 18TH WORD OF                   00009000
             % EACH PRINT LINE RECORD                                   00009100
   ARRAY LINZ[0:17];                                                    00009200
                   %  PRINT LINE BUFFER                                 00009300
   DEFINE PRINTORINHIBIT = D.[18:1] #,                                  00009400
                                        % SEE PAGE 9-45                 00009500
   PRINT = 0 #,                                                         00009600
                                        % B5500 SYSTEMS REF. MANUAL     00009700
   SPACING1 = D.[28:1] #, SPACING2 = D.[27:1] #, DUOBLE = 1 #, SINGLE = 00009800
        1 #, LOOPCONTROL = D.[29: 4] #, LINELENGTH = 15 #;              00009900
                               % LENGTH OF PRINT LINE                   00010000
   DEFINE LL = LINELENGTH #, PAGESKIP = D.[42: 6] #, YES = "~" #;       00010100
   INTEGER STREAM PROCEDURE GETPARS (I, A);                             00010200
% GETPARS =  0  OK                                                      00010300
%            1 NOT P= OR S=                                             00010400
%            2  P=  BAD                                                 00010500
%            3  S=  BAD                                                 00010600
  %  A[1]=1   P= SET                                                    00010700
  %  A[2]=1   B= SET                                                    00010800
  %  A[3]=   SIZE NAME1 CHRS                                            00010900
  %  A[4]=   NAME1                                                      00011000
  %  A[5]=   SIZE NAME2 CHRS                                            00011100
  %  A[6]=   NAME2                                                      00011200
  %  A[7]=   NUMBER LINES TO SKIP                                       00011300
   BEGIN                                                                00011400
      LOCAL T, V;                                                       00011500
      LABEL LA, EXIT, LC, LJ, LX, LZ, LB, LQ;                           00011600
      SI ~ I;                                                           00011700
      SI ~ SI - 1;                                                      00011800
      DI ~ A;                                                           00011900
      DS ~ 56 LIT "0";                                                  00012000
                                  % 7 WORDS ZERO                        00012100
      DI ~ LOC GETPARS;                                                 00012200
      DS ~ 8 LIT "0";                                                   00012300
      LA: SI ~ SI + 1;                                                  00012400
      IF SC = "~" THEN GO TO EXIT;                                      00012500
      IF SC = " " THEN GO TO LA;                                        00012600
      IF SC = "P" THEN                                                  00012700
      BEGIN                                                             00012800
         SI ~ SI + 1;                                                   00012900
         IF SC = "=" THEN                                               00013000
         BEGIN                                                          00013100
            LC: SI ~ SI + 1;                                            00013200
            IF SC = " " THEN GO TO LC;                                  00013300
            IF SC = "~" THEN                                            00013400
            BEGIN                                                       00013500
               LJ: TALLY ~ 2;                                           00013600
               GETPARS ~ TALLY;                                         00013700
               GO TO EXIT;                                              00013800
            END;                                                        00013900
            IF SC = "," THEN GO TO LJ;                                  00014000
            IF SC = "/" THEN GO TO LJ;                                  00014100
            IF SC = ALPHA THEN                                          00014200
            BEGIN                                                       00014300
               TALLY ~ 0;                                               00014400
               7 (SI ~ SI + 1;                                          00014500
               TALLY ~ TALLY + 1;                                       00014600
               IF SC = "~" THEN JUMP OUT 1 TO LJ;                       00014700
               IF SC = "," THEN JUMP OUT 1 TO LJ;                       00014800
               IF SC = "/" THEN JUMP OUT);                              00014900
               T ~ TALLY;                                               00015000
               V ~ SI;                                                  00015100
               SI ~ LOC T;                                              00015200
               DI ~ A;                                                  00015300
               DI ~ DI + 16;                                            00015400
               DS ~ 1 WDS;                                              00015500
               SI ~ V;                                                  00015600
               SI ~ SI - T;                                             00015700
               DS ~ T CHR;                                              00015800
               LZ: IF SC = "/" THEN                                     00015900
               BEGIN                                                    00016000
                  SI ~ SI + 1;                                          00016100
                  IF SC = ALPHA THEN                                    00016200
                  BEGIN                                                 00016300
                     TALLY ~ 0;                                         00016400
                     7 (SI ~ SI + 1;                                    00016500
                     TALLY ~ TALLY + 1;                                 00016600
                     IF SC = "~" THEN JUMP OUT;                         00016700
                     IF SC = " " THEN JUMP OUT;                         00016800
                     IF SC = "," THEN JUMP OUT;                         00016900
                    );                                                  00017000
                     T ~ TALLY;                                         00017100
                     V ~ SI;                                            00017200
                     SI ~ LOC T;                                        00017300
                     DI ~ A;                                            00017400
                     DI ~ DI + 32;                                      00017500
                     DS ~ 1 WDS;                                        00017600
                     SI ~ V;                                            00017700
                     SI ~ SI - T;                                       00017800
                     DS ~ T CHR;                                        00017900
                     DI ~ A;                                            00018000
                     DS ~ 7 LIT "0";                                    00018100
                     DS ~ 1 LIT "1";                                    00018200
                     LX: IF SC = "," THEN GO TO LA;                     00018300
                     IF SC = "~" THEN GO TO EXIT;                       00018400
                     SI ~ SI + 1;                                       00018500
                     GO TO LX;                                          00018600
                  END                                                   00018700
                  ELSE GO TO LJ;                                        00018800
               END;                                                     00018900
               IF SC = "~" THEN GO TO LJ;                               00019000
               IF SC = "," THEN GO TO LJ;                               00019100
               SI ~ SI + 1;                                             00019200
               GO TO LZ;                                                00019300
            END;                                                        00019400
         END;                                                           00019500
         GO TO LJ;                                                      00019600
      END;                                                              00019700
      IF SC = "S" THEN                                                  00019800
      BEGIN                                                             00019900
         SI ~ SI + 1;                                                   00020000
         IF SC = "=" THEN                                               00020100
         BEGIN                                                          00020200
            TALLY ~ 0;                                                  00020300
            LB: SI ~ SI + 1;                                            00020400
            IF SC } "0" THEN                                            00020500
            BEGIN                                                       00020600
               V ~ SI;                                                  00020700
               8 (TALLY ~ TALLY + 1;                                    00020800
               SI ~ SI + 1;                                             00020900
               IF SC = " " THEN JUMP OUT;                               00021000
               IF SC = "~" THEN JUMP OUT;                               00021100
               IF SC = "," THEN JUMP OUT;                               00021200
               IF SC < "0" THEN                                         00021300
               BEGIN                                                    00021400
                  TALLY ~ 3;                                            00021500
                  GETPARS ~ TALLY;                                      00021600
                  JUMP OUT 1 TO EXIT;                                   00021700
               END);                                                    00021800
               T ~ TALLY;                                               00021900
               DI ~ A;                                                  00022000
               DI ~ DI + 48;                                            00022100
               SI ~ V;                                                  00022200
               DS ~ T OCT;                                              00022300
               DI ~ A;                                                  00022400
               DI ~ DI + 8;                                             00022500
               DS ~ 7 LIT "0";                                          00022600
               DS ~ 1 LIT "1";                                          00022700
               LQ: IF SC = "," THEN GO TO LA;                           00022800
               IF SC = "~" THEN GO TO EXIT;                             00022900
               SI ~ SI + 1;                                             00023000
               GO TO LQ;                                                00023100
            END;                                                        00023200
            IF SC = " " THEN GO TO LB;                                  00023300
         END;                                                           00023400
      END;                                                              00023500
      TALLY ~ 1;                                                        00023600
      GETPARS ~ TALLY;                                                  00023700
      EXIT:                                                             00023800
   END GETPARS;                                                         00023900
 % ****************************************************************     00024000
   STREAM PROCEDURE MVE (N, A, B);                                      00024100
                                     % MOVE N WORDS FROM A TO B         00024200
   VALUE N;                                                             00024300
   BEGIN                                                                00024400
      SI ~ A;                                                           00024500
      DI ~ B;                                                           00024600
      DS ~ N WDS;                                                       00024700
   END MVE;                                                             00024800
 % ************************************************************         00024900
   STREAM PROCEDURE SHUFFLE (A, B);                                     00025000
   BEGIN                                                                00025100
      SI ~ A;                                                           00025200
      5 (18 (SI ~ SI + 8));                                             00025300
                                       % SKIP 5 - 18 WORD RECORDS       00025400
      DI ~ B;                                                           00025500
      5 (18 (SI ~ SI - 8);                                              00025600
      DS ~ 18 WDS;                                                      00025700
      18 (SI ~ SI - 8);                                                 00025800
     );                                                                 00025900
                                             % SHUFFLE 5 RECORDS        00026000
                                   % FROM BOTTOM TO TOP                 00026100
   END SHUFFLE;                                                         00026200
 % ************************************************************         00026300
   STREAM PROCEDURE SPLITPRINTIN (PBTAPE, PRINTLINE, DESC, RECORDCOUNT) 00026400
        ;                                                               00026500
 %   MOVE  LL WORDS  FROM PBTAPE  TO PRINTLINE                          00026600
 %  DESC~(DESC ~ PBTAPE[18TH WORD]) & PBTAPE[(LL+1)TH WORD][42:0:6]     00026700
%   RECORDCOUNT ~ PBTAPE[18TH WORD].[36:12]                             00026800
   BEGIN                                                                00026900
      SI ~ PBTAPE;                                                      00027000
      DI ~ PRINTLINE;                                                   00027100
      DS ~ LL WDS;                                                      00027200
      DI ~ DESC;                                                        00027300
      DI ~ DI + 7;                                                      00027400
      DS ~ 1 CHR;                                                       00027500
      DI ~ DI - 8;                                                      00027600
      SI ~ SI + 16;                                                     00027700
      DS ~ 1 LIT "0";                                                   00027800
      DS ~ 6 CHR;                                                       00027900
      SI ~ SI - 1;                                                      00028000
      DI ~ RECORDCOUNT;                                                 00028100
      DI ~ DI + 6;                                                      00028200
      DS ~ 2 CHR;                                                       00028300
   END SPLITPRINTIN;                                                    00028400
 % *****************************************************************    00028500
   BOOLEAN STREAM PROCEDURE LABLE (A);                                  00028600
                                        % TRUE IF A =" LABEL"           00028700
                                     %  AND CHARACTER 33 = QUESTION MRK 00028800
   BEGIN                                                                00028900
      LOCAL T;                                                          00029000
      DI ~ LOC T;                                                       00029100
      DS ~ 6 LIT " LABEL";                                              00029200
      DI ~ DI - 6;                                                      00029300
      SI ~ A;                                                           00029400
      IF 6 SC = DC THEN                                                 00029500
      BEGIN                                                             00029600
         SI ~ A;                                                        00029700
         SI ~ SI + 32;                                                  00029800
         TALLY ~ 12;                                                    00029900
         T ~ TALLY;                                                     00030000
                           % QUESTION MARK                              00030100
         DI ~ LOC T;                                                    00030200
         DI ~ DI + 7;                                                   00030300
         IF 1 SC = DC THEN TALLY ~ 1 ELSE TALLY ~ 0;                    00030400
      END                                                               00030500
      ELSE TALLY ~ 0;                                                   00030600
      LABLE ~ TALLY;                                                    00030700
   END LABLE;                                                           00030800
 % ***************************************************************      00030900
   BOOLEAN STREAM PROCEDURE MATCHID (PARLIST, LINEREC);                 00031000
  %  PARLIST = A[3] - A[6] OF GETPARS                                   00031100
  %  LINEREC = PBT PRINT RECORD   [ 14TH & 15TH WORDS]                  00031200
  %          TRUE  IF NAME MATCHES                                      00031300
   BEGIN                                                                00031400
      LOCAL T;                                                          00031500
      TALLY ~ 0;                                                        00031600
      SI ~ PARLIST;                                                     00031700
      DI ~ LOC T;                                                       00031800
      DS ~ 1 WDS;                                                       00031900
                                              % T CHRS                  00032000
      DI ~ LINEREC;                                                     00032100
      DI ~ DI + 1;                                                      00032200
                                %  SKIP 1ST CHR                         00032300
      IF T SC = DC THEN                                                 00032400
      BEGIN                                                             00032500
         SI ~ PARLIST;                                                  00032600
         SI ~ SI + 16;                                                  00032700
         DI ~ LOC T;                                                    00032800
         DS ~ 1 WDS;                                                    00032900
         DI ~ LINEREC;                                                  00033000
         DI ~ DI + 9;                                                   00033100
         IF T SC = DC THEN TALLY ~ 1;                                   00033200
      END;                                                              00033300
      MATCHID ~ TALLY;                                                  00033400
   END MATCHID;                                                         00033500
 % ***************************************************************      00033600
   BOOLEAN PROCEDURE NEXTREC (FIL, A);                                  00033700
    %  GET THE NEXT LOGICAL LINE RECORD IN ARRAY A  ( 18 WORDS)         00033800
   FILE FIL;                                                            00033900
   ARRAY A[ *];                                                         00034000
   BEGIN                                                                00034100
      LABEL EN, EX;                                                     00034200
      OWN ARRAY C[ 0 : 90];                                             00034300
                          %  NEXTREC TRUE =  END OF FILE                00034400
      ARRAY B[ 0 : 90];                                                 00034500
      INTEGER X;                                                        00034600
      IF X ~ (LOGREC ~ LOGREC + 1) MOD 5 = 0 THEN                       00034700
      BEGIN                                                             00034800
         READ (FIL, 90, B[ *]) [EN];                                    00034900
         SHUFFLE (B, C);                                                00035000
      END;                                                              00035100
      MVE (18, C[ X | 18], A);                                          00035200
      NEXTREC ~ FALSE;                                                  00035300
      GO TO EX;                                                         00035400
      EN: NEXTREC ~ TRUE;                                               00035500
      EX:                                                               00035600
   END NEXTREC;                                                         00035700
 % ************************************************************         00035800
   PROCEDURE SETPARS (PLIST);                                           00035900
   ARRAY PLIST[ 1];                                                     00036000
   BEGIN                                                                00036100
      SWITCH FORMAT X ~ ("INPUT  P= &/OR S="),                          00036200
           ("TRY AGAIN"),                                               00036300
           ("P= BAD"),                                                  00036400
           ("B= BAD");                                                  00036500
      LABEL OVR, EX;                                                    00036600
      INTEGER V, J, R;                                                  00036700
      DEFINE SP = SPO #;                                                00036800
      DEFINE ZZ = WRITE (SPO, *, "  V=  ", V, R);                       00036900
      WRITE (SPO, 7, PLIST[ *]) #;                                      00037000
      R ~ 0;                                                            00037100
      WRITE (SP, X[0]);                                                 00037200
      OVR: READ (SP);                                                   00037300
      IF V ~ GETPARS (SP (0), PLIST) > 0 THEN                           00037400
      BEGIN                                                             00037500
         WRITE (SP, X[ V]);                                             00037600
         IF R ~ R + 1 > 3 THEN GO TO EX;                                00037700
         ZZ;                                                            00037800
         GO TO OVR;                                                     00037900
      END;                                                              00038000
      EX:                                                               00038100
   END SETPARS;                                                         00038200
 % ******************************************************************   00038300
   BOOLEAN PROCEDURE SKIPBT (FIL, R);                                   00038400
   VALUE R;                                                             00038500
   FILE FIL;                                                            00038600
   INTEGER R;                                                           00038700
   BEGIN                                                                00038800
      INTEGER S;                                                        00038900
      LABEL NX, EF, HL;                                                 00039000
      ARRAY T, L[1 : 18];                                               00039100
      REAL D;                                                           00039200
      S ~ 0;                                                            00039300
      SKIPBT ~ FALSE;                                                   00039400
      NX: IF NEXTREC (FIL, RECORD) THEN GO TO EF;                       00039500
      SPLITPRINTIN (RECORD, T, D, LASTRECORDCOUNT);                     00039600
      IF D.[ 18 : 1] = 0 THEN IF S ~ S + 1 > R THEN GO TO HL;           00039700
      GO TO NX;                                                         00039800
      EF:                                                               00039900
      BEGIN                                                             00040000
         FILE XX 18 (1,15);                                             00040100
         FORMAT Z ("END OF FILE WHILE SKIPPING", 2I 7);                 00040200
         WRITE (SPO, Z, S, R);                                          00040300
         SKIPBT ~ TRUE;                                                 00040400
         GO TO HL;                                                      00040500
      END;                                                              00040600
      HL:                                                               00040700
   END SKIPBT;                                                          00040800
 % ****************************************************************     00040900
   PROCEDURE PUTOUT (F, L, D);                                          00041000
   FILE F;                                                              00041100
   ARRAY L[ *];                                                         00041200
                            %  OUTPUT  LINELENGTH WORDS OF L[*] ON      00041300
   REAL D;                                                              00041400
                            %  FILE F USING BITS IN D TO CONTROL ON     00041500
   BEGIN                                                                00041600
      INTEGER I;                                                        00041700
      IF D.[ 18 : 1] = 0 THEN IF I ~ LOOPCONTROL ! 0 THEN WRITE (F[I],  00041800
           LINELENGTH, L[ *]) ELSE IF PAGESKIP = YES THEN WRITE (F[     00041900
           PAGE], LINELENGTH, L[ *]) ELSE IF SPACING1 = DUOBLE THEN     00042000
           WRITE (F[ DBL], LINELENGTH, L[ *]) ELSE IF SPACING2 = SINGLE 00042100
           THEN WRITE (F, LINELENGTH, L[ *]) ELSE WRITE (F[NO],         00042200
           LINELENGTH, L[ *]);                                          00042300
   END PUTOUT;                                                          00042400
 %********************************************************************* 00042500
   BOOLEAN PROCEDURE SEARCH (FIL, P);                                   00042600
   FILE FIL;                                                            00042700
   ARRAY P[1];                                                          00042800
   BEGIN                                                                00042900
      LABEL EF, NX, EX;                                                 00043000
      DEFINE T = RECORD #;                                              00043100
                       %                                                00043200
      SEARCH ~ FALSE;                                                   00043300
      NX: IF NEXTREC (FIL, T) THEN GO TO EF;                            00043400
      IF LABLE (T) THEN IF MATCHID (P[3], T[13]) THEN GO TO EX;         00043500
      GO TO NX;                                                         00043600
      EF:                                                               00043700
      BEGIN                                                             00043800
         FILE XX 18 (1, 15);                                            00043900
         FORMAT Z ("NAME NOT FOUND");                                   00044000
         WRITE (SPO, Z);                                                00044100
         WRITE (SPO, 7, P[ *]);                                         00044200
         SEARCH ~ TRUE;                                                 00044300
         GO TO EX;                                                      00044400
      END;                                                              00044500
      EX:                                                               00044600
   END SEARCH;                                                          00044700
 % ******************************************************************   00044800
%   MAIN  CODE   ************************************************       00044900
   LOGREC ~ - 1;                                                        00045000
                     %                                                  00045100
   OLDSKIP ~ SKIPFACTOR;                                                00045200
   SETPARS (PL);                                                        00045300
   IF PL[1] = 1 THEN                                                    00045400
   BEGIN                                                                00045500
      IF SEARCH (PBT, PL) THEN GO TO EXIT;                              00045600
      LOGREC ~ LOGREC - 1;                                              00045700
      PL[ 7] ~ PL[ 7] + 1;                                              00045800
   END;                                                                 00045900
   L1: IF PL[2] = 1 THEN                                                00046000
   BEGIN                                                                00046100
      IF SKIPBT (PBT, PL[7]) THEN GO TO EXIT;                           00046200
      OLDSKIP ~ PL[ 7];                                                 00046300
      PL[ 2] ~ 0;                                                       00046400
      GO TO LT;                                                         00046500
   END;                                                                 00046600
   IF NEXTREC (PBT, RECORD) THEN GO TO EXIT;                            00046700
   LT: SPLITPRINTIN (RECORD, Q, DSC, RECORDCOUNT);                      00046800
   IF RECORDCOUNT < LASTRECORDCOUNT THEN                                00046900
   BEGIN                                                                00047000
      WHILE TRUE DO                                                     00047100
      BEGIN                                                             00047200
         IF NEXTREC (PBT, RECORD) THEN GO TO EXIT;                      00047300
         SPLITPRINTIN (RECORD, Q, DSC, RECORDCOUNT);                    00047400
         IF LABLE (RECORD) THEN                                         00047500
         BEGIN                                                          00047600
            LASTRECORDCOUNT ~ RECORDCOUNT ~ 0;                          00047700
            GO TO L1;                                                   00047800
         END;                                                           00047900
      END;                                                              00048000
   END;                                                                 00048100
   LASTRECORDCOUNT ~ RECORDCOUNT;                                       00048200
   IF LABLE (RECORD) AND DSC.[ 18: 1] = 0                               00048300
                                                % PRINTED LABEL LINE    00048400
   THEN WRITE (LINE[ PAGE]);                                            00048500
   PUTOUT (LINE, Q, DSC);                                               00048600
   IF OLDSKIP ! SKIPFACTOR THEN                                         00048700
   BEGIN                                                                00048800
      PL[ 2] ~ 1;                                                       00048900
      PL[ 7] ~ OLDSKIP ~ SKIPFACTOR;                                    00049000
   END;                                                                 00049100
   GO TO L1;                                                            00049200
   EXIT: END.                                                           00049300
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
