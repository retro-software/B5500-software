MULTIPLE    000116AA                                                    00000100
                                                                        00000200
  **********************************************************************00000300
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00000400
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000500
  **********************************************************************00000600
                                                                        00000700
   THIS PROGRAM IS A MULTIPLE UTILITY WHICH PROVIDES A FACILITY         00000800
   FOR DUPLICATING A FILE FROM ONE DEVICE TO ANOTHER WITH OR            00000900
   WITHOUT REBLOCKING.                                                  00001000
                                                                        00001100
   OPERATION OF THE MULTIPLE UTILITY REQUIRES A MINIMUM                 00001200
   OF FOUR CARDS AS FOLLOWS:                                            00001300
      1. CC EXECUTE UTILITY/MULTIPLE                                    00001400
      2. CC DATA CRDS                                                   00001500
      3. ONE CONTROL CARD FOR EACH DUMP OR COPY TO BE                   00001600
         PERFORMED IN THE FOLLOWING FORMAT:                             00001700
         COLS. 1 - 2   CONTROL CODE                                     00001800
               3 - 4   RECORDING CODE                                   00001900
               5 -54   PARAMETERS                                       00002000
      4. CC END                                                         00002100
                                                                        00002200
   CODES FOR THE CONTROL CARDS ARE:                                     00002300
      CONTROL CODE   MEANING                                            00002400
      ------------   -------                                            00002500
         CC          CARD TO CARD                                       00002600
         CP          CARD TO PRINT                                      00002700
         CD          CARD TO DISK                                       00002800
         CT          CARD TO TAPE                                       00002900
         DC          DISK TO CARD                                       00003000
         DP          DISK TO PRINTER                                    00003100
         DD          DISK TO DISK                                       00003200
         DT          DISK TO TAPE                                       00003300
         TC          TAPE TO CARD                                       00003400
         TP          TAPE TO PRINT                                      00003500
         TD          TAPE TO DISK                                       00003600
         TT          TAPE TO TAPE                                       00003700
                                                                        00003800
      RECORDING CODE   MEANING                                          00003900
      --------------   -------                                          00004000
           SS          STANDARD TO STANDARD                             00004100
           SN          STANDARD TO NON-STANDARD                         00004200
           NN          NON-STANDARD TO NON-STANDARD                     00004300
           NS          NON-STANDARD TO STANDARD                         00004400
                                                                        00004500
      PARAMETER   MEANING                                               00004600
      ---------   -------                                               00004700
         I        INPUT RECORD LENGTH IN WORDS                          00004800
         B        BLOCKING FACTOR OF INPUT                              00004900
         R        OUTPUT RECORD LENGTH IN WORDS                         00005000
         O        BLOCKING FACTOR OF OUTPUT                             00005100
         N        PREFIX OF INPUT FILE NAME                             00005200
         A        SUFFIX OF INPUT FILE NAME                             00005300
         M        PREFIX OF OUTPUT FILE NAME                            00005400
         E        SUFFIX OF OUTPUT FILE NAME                            00005500
         H        HOW MANY RECORDS ARE TO BE PROCESSED                  00005600
         W        WHICH RECORDS ARE TO BE PROCESSED                     00005700
                                                                        00005800
   NOTE THAT IF THE CONTROL CODE IS CC,CP,CD,DC,DP OR DD,               00005900
   THEN THE RECORDING CODE MUST BE SS.                                  00006000
                                                                        00006100
   IF THE CONTROL CODE IS CT OR DT, THE RECORDING CODE MAY BE           00006200
   EITHER SS OR SN.                                                     00006300
                                                                        00006400
   IF THE CONTROL CODE IS TC,TD,OR TP, THE RECORDING CODE MAY           00006500
   BE SS OR NS.                                                         00006600
                                                                        00006700
   WITH A CONTROL CODE OF TT, ANY RECORDING CODE IS PERMITTED.          00006800
                                                                        00006900
   THE RECORDING CODE MUST AGREE WITH THE RECORDING MODE OF THE         00007000
   INPUT AND OUTPUT FILES. IF IT IS LEFT BLANK, THE RECORDING           00007100
   CODE SS WILL BE ASSUMED.                                             00007200
                                                                        00007300
   PARAMETER W INDICATES WHICH RECORDS ARE TO BE PROCESSED.             00007400
   E.G., W5 CAUSES EVERY FIFTH RECORD TO BE PROCESSED.                  00007500
                                                                        00007600
   THE PARAMETERS IN COLS. 5-54 ARE FREE FORM AND MAY BE LISTED         00007700
   IN ANY SEQUENCE. EACH OF THESE PARAMETERS MUST BE PRECEDED           00007800
   BY ONE BLANK OR ONE COMMA. THE LAST PARAMETER MUST BE                00007900
   FOLLOWED BY A PERIOD.                                                00008000
                                                                        00008100
   INPUT RECORD LENGTH IS NOT REQUIRED FOR CARD INPUT AND IF            00008200
   ENTERED WILL BE IGNORED. FOR DISK AND TAPE INPUT, RECORD             00008300
   LENGTH MUST BE SPECIFIED.                                            00008400
                                                                        00008500
   OUTPUT RECORD LENGTH IS NOT REQUIRED, AND IF ENTERED WILL BE         00008600
   IGNORED. NOTE THAT THIS UTILITY WILL NOT CHANGE RECORD LENGTHS.      00008700
                                                                        00008800
   OUTPUT RECORD LENGTH FOR PRINTER OUTPUT WILL BE INFLUENCED BY        00008900
   INPUT RECORD LENGTH. IF THE INPUT RECORD LENGTH IS NOT GREATER       00009000
   THAN 132, THEN THE PRINTER RECORDS WILL BE THE SAME LENGTH AS        00009100
   THE INPUT RECORD. IF THE INPUT RECORD LENGTH IS GREATER THAN         00009200
   132, THEN 100 CHARACTERS WILL BE PRINTED ON EACH LINE OF OUTPUT.     00009300
   OUTPUT RECORD LENGTH FOR CARDS WILL ALWAYS BE ASSUMED TO BE          00009400
   10 WORDS. THE INPUT RECORD LENGTH MAY BE LESS THAN 10 WORDS          00009500
   AND CORRECT RESULTS WILL BE OBTAINED.                                00009600
                                                                        00009700
   BLOCKING FACTOR FOR INPUT WILL BE ASSUMED TO BE ONE FOR              00009800
   CARD OR WHEN BLOCKING FACTOR IS OMITTED.                             00009900
                                                                        00010000
   BLOCKING FACTOR FOR OUTPUT WILL BE ASSUMED TO BE 1 FOR CARD,         00010100
   PRINTER OR WHEN OUTPUT AND INPUT BLOCKING ARE NOT SPECIFIED.         00010200
   WHEN INPUT BLOCKING IS SPECIFIED (OR 1 WAS ASSUMED), OUTPUT          00010300
   BLOCKING IS NOT SPECIFIED AND THE OUTPUT DEVICE IS TAPE OR           00010400
   DISK, THEN THE OUTPUT BLOCKING IS ASSUMED TO BE THE SAME AS          00010500
   THE INPUT BLOCKING.                                                  00010600
                                                                        00010700
   IF A BLOCKING FACTOR IS SPECIFIED FOR INPUT CARD, OUTPUT CARD        00010800
   OR PRINTER, IT WILL BE IGNORED.                                      00010900
                                                                        00011000
   ALL OUTPUT FILES WILL HAVE STANDARD BURROUGHS LABELS. IBM            00011100
   HEADER LABELS (FIRST 4 CHARACTERS "1HDR) WILL NEVER BE COPIED        00011200
   TO AN OUTPUT FILE.                                                   00011300
                                                                        00011400
   FOR AN INPUT OR AN OUTPUT CARD FILE, THE FILE IDENTIFIER             00011500
   0000000/CARD IS ASSUMED AND ANY SPECIFICATION IS IGNORED.            00011600
   THE FILE IDENTIFIER (PREFIX AND SUFFIX) MUST BE SPECIFIED            00011700
   FOR ALL OTHER INPUT FILES.                                           00011800
                                                                        00011900
   FOR AN OUTPUT PRINT FILE, THE FILE IDENTIFIER 0000000/PRINT          00012000
   IS ASSUMED. FOR OUTPUT DISK OR TAPE FILES FOR WHICH NO FILE          00012100
   IDENTIFIER IS SPECIFIED, THE INPUT IDENTIFIER WILL BE USED.          00012200
   IF THE CONTROL CODE SPECIFIES DD (DISK TO DISK), THEN THE            00012300
   OUTPUT FILE IDENTIFIER MUST BE SPECIFIED.                            00012400
                                                                        00012500
   IF NO PARAMETERS ARE TO BE SPECIFIED, THEN COL. 5 MUST CONTAIN       00012600
   A PERIOD.                                                            00012700
                                                                        00012800
   THE EQUIVALENT OF THE FOUR CARDS MAY BE ENTERED THRU THE SPO         00012900
   AS FOLLOWS:                                                          00013000
      1. CC EXECUTE UTILITY/MULTIPLE;END                                00013100
      2. (MIX INDEX) IN25 = 2                                           00013200
      3. CONTROL CARD INFORMATION (MUST END WITH PERIOD)                00013300
                                                                        00013400
   CONTROL CARD EXAMPLE:                                                00013500
      DTSN,I30,B5,R30,B2,NTEST,AFILE.                                   00013600
                                                                        00013700
                                                                        00013800
OPERATING INSTRUCTIONS********************                              00013900
   EXECUTION MAY BE FROM CARD OR SPO                                    00014000
   1. FROM CARD                                                         00014100
      LOAD THE "EXECUTE","DATA", A MAXIMUM OF 10 CONTROL CARDS          00014200
      AND AN "END" CARD IN THE CARD READER.                             00014300
                                                                        00014400
      FOLLOWING THE MESSAGE: "UTILITY/MULTIPL=N;ACCEPT"                 00014500
      TYPE IN: (MIX) AXGO                                               00014600
                                                                        00014700
      WHEN ALL OPERATIONS (ONE FROM EACH CONTROL CARD)                  00014800
      ARE COMPLETE, THE PROGRAM WILL LOOP FOR 10 SECONDS                00014900
      BEFORE COMING TO AN END-OF-JOB. DURING THIS 10 SECONDS,           00015000
      AN ADDITIONAL SET OF CARDS ("DATA",CONTROL, AND "END") MAY        00015100
      BE FED THRU THE CARD READER -- OR ADDITIONAL OPERATIONS MAY       00015200
      BE INITIATED VIA THE SPO.                                         00015300
                                                                        00015400
   2. FROM SPO                                                          00015500
      INITIATE THE PROGRAM EITHER FROM A "CC EXECUTE" ON THE            00015600
      SPO OR A "CC EXECUTE" CARD IN THE READER.                         00015700
                                                                        00015800
      WITHIN 10 SECONDS, TYPE IN (MIX)IN25=2.                           00015900
                                                                        00016000
      FOLLOWING THE MESSAGE: "UTILITY/MULTIPL=N:ACCEPT"                 00016100
      TYPE IN: "(MIX)AX"  FOLLOWED BY WHAT WOULD NORMALLY               00016200
      APPEAR IN A CONTROL CARD. TYPE IN MUST END WITH A                 00016300
      PERIOD.                                                           00016400
                                                                        00016500
      FOLLOWING THE SECOND OCCURRENCE OF THE MESSAGE:                   00016600
      "UTILITY/MULTIPL=N:ACCEPT", TYPE IN "(MIX)AXGO".                  00016700
                                                                        00016800
                                                                        00016900
BEGIN                                                                   00017000
                                                                        00017100
   COMMENT A GENERAL UTILITY PROGRAM TO COPY ANY FILE TO ANOTHER FILE   00017200
           WRITTEN BY E. A. SPELICH, BURROUGHS SAN JOSE, CALIFORNIA ;   00017300
                                                                        00017400
   INTEGER PRT25,SECS,S1,S2,INBUF,OTBUF;                                00017500
   INTEGER I,MAX;                                                       00017600
   FILE IN CRDS(2,10);                                                  00017700
   FILE OUT PRNT 4(2,15);                                               00017800
   FILE SPO 11(1,10);                                                   00017900
   FORMAT FMT1(A2);                                                     00018000
   ARRAY GBG[00:10,0:09],INFO[0:15],CODES[0:4];                         00018100
   ALPHA GOGO;                                                          00018200
   LABEL LAB1,EOF,PROCRDS,LABGO;                                        00018300
   DEFINE                                                               00018400
          TYPE    =  INFO[0]  #,                                        00018500
          MODE    =  INFO[1]  #,                                        00018600
          INREC   =  INFO[2]  #,    % I                                 00018700
          INBLK   =  INFO[3]  #,    % B                                 00018800
          OTREC   =  INFO[4]  #,    % R                                 00018900
          OTBLK   =  INFO[5]  #,    % O                                 00019000
          NURCRD  =  INFO[6]  #,    % H                                 00019100
          WHRCRD  =  INFO[7]  #,    % W                                 00019200
          INNAMS  =  INFO[8]  #,    % N                                 00019300
          INNAME  =  INFO[9]  #,    % A                                 00019400
          OTNAMS  =  INFO[10] #,    % M                                 00019500
          OTNAME  =  INFO[11] #,    % E                                 00019600
          XX      =  INFO[12]  #,    % EXTRA                            00019700
          YY      =  INFO[13]  #,    % EXTRA                            00019800
          ZZ      =  INFO[14]  #,    % EXTRA                            00019900
          WW      =  INFO[15]  #;    % EXTRA                            00020000
   LIST L1(GOGO);                                                       00020100
   LIST L2(OTREC,OTBLK,ZZ,OTNAMS,WW,OTNAME);                            00020200
   LIST L3(INREC,INBLK,XX,INNAMS,YY,INNAME);                            00020300
   LIST L4(INREC,INBLK,XX,INNAMS,YY,INNAME,                             00020400
           OTREC,OTBLK,ZZ,OTNAMS,WW,OTNAME);                            00020500
   SWITCH LIST SPOLIST~                                                 00020600
     L1,L2,L1,L2,L3,L4,L3,L4,L3,L4,L3,L4;                               00020700
   SWITCH FORMAT SPOMESS~                                               00020800
     ("CARD TO CARD",A1,"~"),                                           00020900
     ("CARD TO DISK",2I4,X1,A1,A6,"/",A1,A6,"~"),                       00021000
     ("CARD TO PRINT",A1,"~"),                                          00021100
     ("CARD TO TAPE",2I4,X1,A1,A6,"/",A1,A6,"~"),                       00021200
     ("DISK TO CARD",2I4,X1,A1,A6,"/",A1,A6,"~"),                       00021300
     ("DISK TO DISK",2I4,X1,A1,A6,"/",A1,A6,2I4,X1,A1,A6,"/"A1,A6,"~"), 00021400
     ("DISK TO PRINT",2I4,X1,A1,A6,"/",A1,A6,"~"),                      00021500
     ("DISK TO TAPE",2I4,X1,A1,A6,"/",A1,A6,2I4,X1,A1,A6,"/"A1,A6,"~"), 00021600
     ("TAPE TO CARD",2I4,X1,A1,A6,"/",A1,A6,"~"),                       00021700
     ("TAPE TO DISK",2I4,X1,A1,A6,"/",A1,A6,2I4,X1,A1,A6,"/"A1,A6,"~"), 00021800
     ("TAPE TO PRINT",2I4,X1,A1,A6,"/",A1,A6,"~"),                      00021900
     ("TAPE TO TAPE",2I4,X1,A1,A6,"/",A1,A6,2I4,X1,A1,A6,"/"A1,A6,"~"); 00022000
STREAM PROCEDURE MOVEDOWN(A,B,C);                                       00022100
   VALUE C;                                                             00022200
     BEGIN                                                              00022300
   LOCAL X;                                                             00022400
       SI~LOC C;                                                        00022500
       SI~SI+6;                                                         00022600
       IF SC!"0" THEN                                                   00022700
         BEGIN                                                          00022800
           DI~LOC X;                                                    00022900
           DI~DI+7;                                                     00023000
           DS~CHR;                                                      00023100
           SI~A;                                                        00023200
           DI~B;                                                        00023300
           X(2(DS~32 WDS));                                             00023400
         END ELSE                                                       00023500
         BEGIN                                                          00023600
           SI~A;                                                        00023700
           DI~B;                                                        00023800
         END;                                                           00023900
       DS~C WDS;                                                        00024000
     END MOVEDOWN;                                                      00024100
BOOLEAN STREAM PROCEDURE SCAN(A,B,C);                                   00024200
     BEGIN                                                              00024300
   LOCAL X,Y,Z;                                                         00024400
     LABEL L,L1,L2,L3,ALL;                                              00024500
       SI~A; DI~B;                                                      00024600
       DI~DI+6;                                                         00024700
       DS~2 CHR;                                                        00024800
       DI~DI+6;                                                         00024900
       DS~2 CHR;                                                        00025000
   L:                                                                   00025100
       IF SC="." THEN BEGIN TALLY~1; GO TO ALL END;                     00025200
       TALLY~0;                                                         00025300
       SI~SI+1;                                                         00025400
       X~SI;                                                            00025500
       SI~SI+1;                                                         00025600
       8(IF SC="." THEN JUMP OUT TO L1;                                 00025700
         IF SC="," THEN JUMP OUT TO L1;                                 00025800
         IF SC=" " THEN JUMP OUT TO L1;                                 00025900
         SI~SI+1;                                                       00026000
         TALLY~TALLY+1);                                                00026100
       TALLY~0;                                                         00026200
       GO TO ALL;                                                       00026300
   L1:                                                                  00026400
       Y~TALLY;                                                         00026500
       SI~X; DI~C; TALLY~2;                                             00026600
       6(IF SC=DC THEN JUMP OUT TO L2;                                  00026700
         SI~SI-1;                                                       00026800
         TALLY~TALLY+1);                                                00026900
       4(IF SC=DC THEN JUMP OUT TO L3;                                  00027000
         SI~SI-1;                                                       00027100
         TALLY~TALLY+1);                                                00027200
       TALLY~0;                                                         00027300
       GO TO ALL;                                                       00027400
   L2:                                                                  00027500
       Z~TALLY;                                                         00027600
       DI~B;                                                            00027700
       Z(DI~DI+8);                                                      00027800
       DS~Y OCT;                                                        00027900
       GO TO L;                                                         00028000
   L3:                                                                  00028100
       Z~TALLY;                                                         00028200
       DI~B;                                                            00028300
       Z(DI~DI+8);                                                      00028400
       DI~DI+1;                                                         00028500
       X~DI;                                                            00028600
       DS~7 LIT " ";                                                    00028700
       DI~DI-7;                                                         00028800
       DS~Y CHR;                                                        00028900
       Y~SI;                                                            00029000
       SI~X;                                                            00029100
       DI~X;                                                            00029200
       4(DI~DI+8);                                                      00029300
       DI~DI+6;                                                         00029400
       DS~CHR;                                                          00029500
       SI~Y;                                                            00029600
       GO TO L;                                                         00029700
   ALL:                                                                 00029800
       SCAN~TALLY;                                                      00029900
     END OF SCAN;                                                       00030000
STREAM PROCEDURE BLANKR(A,B);                                           00030100
     BEGIN                                                              00030200
   LABEL L;                                                             00030300
       SI~B;                                                            00030400
       SI~SI+7;                                                         00030500
       IF SC="1" THEN GO TO L;                                          00030600
       SI~A;                                                            00030700
       SI~SI+4;                                                         00030800
       DI~A;                                                            00030900
       13(DS~8 CHR);                                                    00031000
   L:                                                                   00031100
       DI~A;                                                            00031200
       12(DI~DI+8);                                                     00031300
       DI~DI+4;                                                         00031400
       DS~4 LIT " ";                                                    00031500
     END OF BLANKR;                                                     00031600
BOOLEAN STREAM PROCEDURE CODER(A,B);                                    00031700
     BEGIN                                                              00031800
   LOCAL X;                                                             00031900
       SI~A; DI~B;                                                      00032000
       SI~SI+6;                                                         00032100
       DI~DI+10;                                                        00032200
       12(IF 2 SC=DC THEN                                               00032300
          BEGIN                                                         00032400
            X~TALLY; TALLY~1; CODER~TALLY;                              00032500
            JUMP OUT;                                                   00032600
          END;                                                          00032700
          TALLY~TALLY+1;                                                00032800
          SI~SI-2);                                                     00032900
       DI~A; SI~LOC X;                                                  00033000
       DS~WDS;                                                          00033100
     END CODER;                                                         00033200
PROCEDURE RUNIT(FILEIN,FILEOUT);                                        00033300
   FILE FILEIN,FILEOUT;                                                 00033400
     BEGIN                                                              00033500
BOOLEAN STREAM PROCEDURE DESTROYIBM(A);                                 00033600
   BEGIN                                                                00033700
      LOCAL X;                                                          00033800
     DI~LOC X;                                                          00033900
     DS~5 LIT"1HDR ";                                                   00034000
     DI~ LOC X;                                                         00034100
     SI~A;                                                              00034200
     IF 5 SC=DC THEN TALLY~1;                                           00034300
     DESTROYIBM~TALLY;                                                  00034400
   END OF DESTROYIBM;                                                   00034500
STREAM PROCEDURE BLANKIT(A,B);                                          00034600
   VALUE B;                                                             00034700
   BEGIN                                                                00034800
      SI~A;                                                             00034900
      DI~A;                                                             00035000
      DS~8 LIT " ";                                                     00035100
      DS~B WDS;                                                         00035200
   END;                                                                 00035300
   INTEGER IJK;                                                         00035400
   INTEGER WDS,INCOUNT,OUTCOUNT,INX,OUTX;                               00035500
   BOOLEAN INREADY,BOOL;                                                00035600
   ARRAY INBUFR[0:INREC],OTBUFR[0:OTREC];                               00035700
   LABEL AGAIN,EOF,PAR, READTEST;                                       00035800
   FORMAT REC(/ "* * RECORD ",I4 /);                                    00035900
   FORMAT RECORDS("INPUT COUNT =",I6,"...OUTPUT COUNT =",I6,"~");       00036000
   FORMAT PARITY("INPUT PARITY * TO CONTINUE ENTER GO ~");              00036100
       INREADY~TRUE;                                                    00036200
   AGAIN:                                                               00036300
       IF INREADY THEN                                                  00036400
         BEGIN                                                          00036500
          READTEST:                                                     00036600
           READ(FILEIN,INREC,INBUFR[*])[EOF:PAR];                       00036700
         IF S2!2 AND                                                    00036800
            (DESTROYIBM(INBUFR   )) THEN GO TO READTEST;                00036900
           INCOUNT~INCOUNT+1;                                           00037000
           INX~0;                                                       00037100
           BOOL~FALSE;                                                  00037200
         IF S2=2 AND S1!0 AND INCOUNT MOD WHRCRD=0 THEN                 00037300
           WRITE(FILEOUT,REC,INCOUNT);                                  00037400
         END;                                                           00037500
       MOVEDOWN(INBUFR[INX],OTBUFR[OUTX],WDS~                           00037600
                IF (WDS~INREC-INX)}OTREC THEN OTREC ELSE WDS);          00037700
       INREADY~                                                         00037800
         IF ((INX~                                                      00037900
           IF (INX~INX+OTREC}INREC) THEN 0 ELSE INX                     00038000
                 )=0) THEN TRUE ELSE FALSE;                             00038100
       IF ((OUTX~                                                       00038200
         IF (OUTX~OUTX+INREC}OTREC) THEN 0 ELSE OUTX                    00038300
                )=0) THEN                                               00038400
                     BEGIN                                              00038500
                       IF S2=2 THEN                                     00038600
                         BEGIN                                          00038700
                           IF INREC>17 THEN                             00038800
                             BEGIN                                      00038900
                               BOOL~NOT BOOL;                           00039000
                               IF BOOL THEN INX~INX-1;                  00039100
                               WDS~IF BOOL THEN 1 ELSE 0;               00039200
                               BLANKR(OTBUFR,WDS);                      00039300
                             END;                                       00039400
                         END;                                           00039500
                       IF INCOUNT MOD WHRCRD=0 THEN                     00039600
                         BEGIN                                          00039700
                           WRITE(FILEOUT,OTREC,OTBUFR[*]);              00039800
                           IJK~OTREC-1;                                 00039900
                           BLANKIT(OTBUFR,IJK);                         00040000
                           OUTCOUNT~OUTCOUNT+1;                         00040100
                         END;                                           00040200
                       IF OUTCOUNT=NURCRD THEN GO TO EOF;               00040300
                     END;                                               00040400
       GO TO AGAIN;                                                     00040500
   PAR:                                                                 00040600
       WRITE(SPO,PARITY);                                               00040700
       READ(SPO,FMT1,GOGO);                                             00040800
       IF GOGO="GO" THEN GO TO AGAIN;                                   00040900
   EOF:                                                                 00041000
       WRITE(SPO,RECORDS,INCOUNT,OUTCOUNT);                             00041100
       IF S2=2 THEN WRITE(FILEOUT,RECORDS,INCOUNT,OUTCOUNT);            00041200
       IF S2=1 THEN LOCK(FILEOUT,SAVE);                                 00041300
       CLOSE(FILEIN,RELEASE);                                           00041400
       CLOSE(FILEOUT,RELEASE);                                          00041500
     END RUNIT;                                                         00041600
PROCEDURE EXECUTE;                                                      00041700
     BEGIN                                                              00041800
   FILE IN CARDIN(2,10);                                                00041900
   FILE IN DISKIN DISK SERIAL(2,INREC,INBUF);                           00042000
   FILE IN TAPEIN(2,INREC,INBUF);                                       00042100
   ALPHA FILE IN ATAPIN(2,INREC,INBUF);                                 00042200
   FILE OUT CARDOUT 0(2,10);                                            00042300
   FILE OUT DISKOUT DISK SERIAL[20:(OTREC|OTBLK)|10]  (2,OTREC,OTBUF,   00042400
           SAVE 5);                                                     00042500
   FILE OUT TAPEOUT(2,OTREC,OTBUF,SAVE 5);                              00042600
   ALPHA FILE OUT ATAPOUT(2,OTREC,OTBUF,SAVE 5);                        00042700
   FILE OUT PRINT 18(2,17);                                             00042800
   SWITCH FILE SWF1~CARDIN,DISKIN,TAPEIN,ATAPIN;                        00042900
   SWITCH FILE SWF2~CARDOUT,DISKOUT,PRINT,TAPEOUT,ATAPOUT;              00043000
   FILL SWF1[S1] WITH INNAMS,INNAME;                                    00043100
   FILL SWF2[S2] WITH OTNAMS,OTNAME;                                    00043200
       RUNIT(SWF1[S1],SWF2[S2]);                                        00043300
     END EXECUTE;                                                       00043400
PROCEDURE INERR(A);                                                     00043500
   VALUE A;                                                             00043600
   INTEGER A;                                                           00043700
     BEGIN                                                              00043800
   FORMAT FMT(" INVALID CONTROL CARD ~");                               00043900
   FORMAT FMTT(//"INVALID CONTROL CARD",                                00044000
               / "         1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 5 5 "00044100
                 "5 5 5 6 6 6 6 6 7 7 7 7 7 8 "         /               00044200
                 " 2 4 6 8 0 2 4 6 8 0 2 4 6 8 0 2 4 6 8 0 2 4 6 8 0 2 "00044300
                 "4 6 8 0 2 4 6 8 0 2 4 6 8 0 " );                      00044400
       WRITE(SPO,FMT);                                                  00044500
       WRITE(PRNT,FMTT);                                                00044600
       WRITE(PRNT,10,GBG[A,*]);                                         00044700
     END INERR;                                                         00044800
   FILL CODES[*] WITH "IBROHWNA","MECCCDCP","CTDCDDDP","DTTCTDTP",      00044900
                      "TT000000";                                       00045000
       SECS~10;     % NUMBER OF SECONDS PROGRAM WILL WAIT BEFORE GOING  00045100
                    % TO EOJ.  TO CHANGE ENTER NEW VALUE INTO PRT 26.   00045200
       PRT25~77;                                                        00045300
   LAB1:                                                                00045400
       IF PRT25!0 AND PRT25!77 THEN                                     00045500
         BEGIN                                                          00045600
           FOR I~0 STEP 1 UNTIL 15 DO INFO[I]~0;                        00045700
           IF PRT25}10 THEN GO TO PROCRDS;                              00045800
           IF PRT25=2 THEN                                              00045900
             BEGIN                                                      00046000
               PRT25~I~0;                                               00046100
               READ(SPO,10,GBG[0,*]);                                   00046200
               IF SCAN(GBG[0,*],INFO,CODES) AND CODER(TYPE,CODES) THEN  00046300
               GO TO LABGO ELSE INERR(0);                               00046400
               GO TO LAB1;                                              00046500
             END ELSE                                                   00046600
               FOR I~0 STEP 1 UNTIL 10 DO                               00046700
                   READ(CRDS,10,GBG[I,*])[EOF];                         00046800
   EOF:                                                                 00046900
       CLOSE(CRDS,RELEASE);                                             00047000
       MAX~I-1;                                                         00047100
       PRT25~11;                                                        00047200
   PROCRDS:                                                             00047300
       IF (I~PRT25 MOD 11)>MAX THEN                                     00047400
         BEGIN                                                          00047500
           PRT25~0;                                                     00047600
           GO TO LAB1;                                                  00047700
         END;                                                           00047800
       PRT25~PRT25+1;                                                   00047900
       IF SCAN(GBG[I,*],INFO,CODES) AND CODER(TYPE,CODES) THEN          00048000
       GO TO LABGO ELSE INERR(I);                                       00048100
       GO TO LAB1;                                                      00048200
   LABGO:                                                               00048300
           S1~TYPE DIV 4;                                               00048400
           IF S1=2 AND (MODE="NN" OR MODE="NS") THEN S1~3;              00048500
           S2~TYPE MOD 4;                                               00048600
           IF S2=3 AND (MODE="NN" OR MODE="SN") THEN S2~4;              00048700
           IF S1=0 THEN                                                 00048800
             BEGIN                                                      00048900
               INNAME~"CARDS  ";                                        00049000
               INREC~10;                                                00049100
             END ELSE                                                   00049200
             BEGIN                                                      00049300
               IF INNAME=0 THEN                                         00049400
               IF OTNAME!0 THEN                                         00049500
               INNAME~OTNAME ELSE BEGIN INERR(I); GO TO LAB1 END;       00049600
               IF INREC=0 THEN                                          00049700
               IF OTREC!0 THEN                                          00049800
               INREC~OTREC ELSE BEGIN INERR(I); GO TO LAB1 END;         00049900
             END;                                                       00050000
           IF S2=0 THEN                                                 00050100
             BEGIN                                                      00050200
               OTNAME~"CARD";                                           00050300
               OTREC~10;                                                00050400
             END ELSE                                                   00050500
           IF S2=2 THEN                                                 00050600
             BEGIN                                                      00050700
               OTNAME~"PRINT";                                          00050800
               OTREC~                                                   00050900
                 IF OTREC!0 THEN OTREC ELSE                             00051000
                 IF INREC!0 AND INREC<17 THEN INREC ELSE 13;            00051100
             END;                                                       00051200
           BEGIN                                                        00051300
             IF OTNAME=0 THEN                                           00051400
             IF INNAME!0 THEN                                           00051500
             OTNAME~INNAME ELSE   BEGIN INERR(I); GO TO LAB1 END;       00051600
             IF OTREC=0 THEN                                            00051700
             IF INREC!0 THEN                                            00051800
             OTREC~INREC ELSE BEGIN INERR(I); GO TO LAB1 END;           00051900
           END;                                                         00052000
           INBUF~(INBLK~IF INBLK=0 THEN 1 ELSE INBLK)|INREC;            00052100
           OTBUF~(OTBLK~IF OTBLK=0 THEN 1 ELSE OTBLK)|OTREC;            00052200
           NURCRD~IF NURCRD=0 THEN @68 ELSE NURCRD;                     00052300
           WHRCRD~IF WHRCRD=0 THEN 1 ELSE WHRCRD;                       00052400
       WRITE(SPO,SPOMESS[TYPE],SPOLIST[TYPE]);                          00052500
       READ(SPO,FMT1,GOGO);                                             00052600
       IF GOGO="GO" THEN                                                00052700
         EXECUTE;                                                       00052800
     END;                                                               00052900
       PRT25~IF PRT25=77 THEN 1 ELSE PRT25;                             00053000
       WHEN(SECS);                                                      00053100
       IF PRT25!0 THEN GO TO LAB1;                                      00053200
END.                                                                    00053300
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
