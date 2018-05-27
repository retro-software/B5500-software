%.......................D I S C A R.....................................00000100
COMMENT DOCUMENT                                                        00000200
* PAGE CENTER SKIP 15 UNDERLINE                                         00000300
D I S C A R                                                             00000400
* CENTER SKIP 2                                                         00000500
A B-5500 UTILITY PROGRAM FOR                                            00000600
PRINTING AND PUNCHING DISK AND TAPE FILES                               00000700
* SKIP 10                                                               00000800
ORIGINAL VERSION BY MURRAY FREEDMAN                                     00000900
MODIFICATIONS AND ADDITIONS BY                                          00001000
* INDENT 10                                                             00001100
MURRAY FREEDMAN                                                         00001200
VEER VASISHTA                                                           00001300
BILL MOIR                                                               00001400
* SKIP 2                                                                00001500
BURROUGHS BUSINESS MACHINES LTD                                         00001600
TORONTO SPECIAL ACCOUNTS BRANCH                                         00001700
752 BAY STREET                                                          00001800
TORONTO 101 ONTARIO                                                     00001900
* PAGE UNDERLINE                                                        00002000
INTRODUCTION                                                            00002100
* DEFINE 1 = SKIP PARAGRAPH 1,72,0 *                                    00002200
* 1                                                                     00002300
DISCAR IS A MEDIA-TO-MEDIA UTILITY FOR THE B-5500.  THE ORIGINAL        00002400
VERSION OF THE PROGRAM WAS WRITTEN AS A DISK TO PRINTER UTILITY,        00002500
AND HAS SINCE BEEN, AND IS BEING, EXPANDED TO ENCOMPASS VIRTUALLY ANY   00002600
MEDIA TO MEDIA REQUIREMENT.                                             00002700
* 1                                                                     00002800
THE PROGRAM HAS BEEN DESIGNED WITH CERTAIN FUNDAMENTAL SPECIFICATIONS:  00002900
* DEFINE 2 = PARAGRAPH - 5,72,0           *                             00003000
* 2                                                                     00003100
1.  PRINTED OUTPUT SHOULD BE, TO THE FULLEST EXTENT POSSIBLE,           00003200
COMPLETELY FORMATTED, NEAT, LEGIBLE, AND EASY TO FOLLOW - RATHER THAN   00003300
THE                                                                     00003400
CHARACTERISTIC UNFORMATTED BLIND TRANSCRIPTIONS OF MOST SUCH UTILITIES. 00003500
* 2                                                                     00003600
2.  THE PROGRAMS USEFULNESS AND APPLICATION SHOULD BE AS UNIVERSAL AS   00003700
POSSIBLE.                                                               00003800
*2                                                                      00003900
3.  THE PROGRAM SHOULD BE ABSOLUTELY SIMPLE TO OPERATE.                 00004000
* 2                                                                     00004100
4.  PARAMETERS SHOULD BE AT AN ABSOLUTE BARE MINIMUM.                   00004200
* 2                                                                     00004300
5.  WHATEVER PARAMETERS ARE REQUIRED SHOULD BE FREE-FORM, SIMPLE,       00004400
AND LOGICAL.                                                            00004500
* 2                                                                     00004600
6.  THE OPERATOR SHOULD HAVE THE ABILITY TO SIMPLY ALTER THE FORMAT OF  00004700
PRINTOUTS IN BEING ABLE TO SWITCH BETWEEN SINGLE AND DOUBLE SPACING,    00004800
SIX AND EIGHT LINES PER INCH, WIDE AND NARROW PAPER, ET CETERA.         00004900
* PAGE UNDERLINE                                                        00005000
GENERAL OPERATION                                                       00005100
* SKIP                                                                  00005200
* 1                                                                     00005300
THE PROGRAM RECEIVES ITS CONTROL AND OPERATION INSTRUCTIONS THRU SPO    00005400
OR (ALTERNATELY) CARD-READER INPUT.  SPO OPERATION ONLY WILL BE DISCUSSE00005500
D AS CARD-READER OPERATION IS MERELY A MATTER OF REPRESENTING EACH SPO  00005600
MESSAGE ON A SEPARATE CARD ON A CARD FILE.                              00005700
* 1                                                                     00005800
THE OPERATOR COMMUNICATES WITH THE PROGRAM VIA FREE-FORM MESSAGES IN    00005900
THE FORM OF TRADITIONAL B-5500 FILE-NAMES, VIZ <MFID>/<ID>.             00006000
THE PROGRAM RECOGNIZES BOTH ACTUAL FILE-NAMES OR GROUPS AND             00006100
PSEUDO-FILE-NAMES WHICH ARE USED FOR CONTROL INSTRUCTIONS FOR THE       00006200
PROGRAM.                                                                00006300
* 1                                                                     00006400
ANY INPUT MESSAGE NOT RECOGNIZED AS A CONTROL INSTRUCTION IS            00006500
CONSIDERED AN ACTUAL      FILE-NAME.  A   DISK FILE-NAME CAN TAKE ON    00006600
ANY                                                                     00006700
OF THE FORMS USED IN THE NORMAL "PD" MESSAGE WITH THE SAME MEANING      00006800
BEING IMPLIED.  THEREFORE A DISK FILE-NAME MAY BE <MFID>/<ID> OR        00006900
<MFID>/= OR =/<ID> OR =/=.                                              00007000
* 1                                                                     00007100
EITHER A      FILE-NAME OR GROUP OR A PSEUDO FILE-NAME MUST BE          00007200
ENTERED ON THE SPO VIA <MIX> AX <FILE-NAME> EACH TIME THE PROGRAM       00007300
TYPES ON THE SPO "FILE PLEASE" FOLLOWED BY AN MCP ACCEPT REQUEST.       00007400
* 1                                                                     00007500
IN ORDER TO TERMINATE PREMATURELY THE PROCESSING OF ANY GIVEN FILE,     00007600
THE OPERATOR MAY TYPE IN AT THE SPO "<MIX> IN 25 = 1".  THIS WILL CAUSE 00007700
A BOOLEAN VARIABLE "COMMITSUICIDE" TO BE SET TRUE.  THE PROGRAM CHECKS  00007800
COMMITSUICIDE ON EVERY PRINT CYCLE AND WILL TERMINATE THE PROCESSING    00007900
OF THE CURRENT FILE WHEN TRUE.  NOTE THAT ON DISK FILE GROUPS ONLY      00008000
THE CURRENT FILE WILL BE SUICIDED.  IF IN TAPE MODE, HOWEVER,           00008100
THE PROGRAM WILL CLOSE OFF THE TAPE AND REQUEST A NEW "FILE PLEASE".    00008200
THEREFORE, THIS CANNOT BE USED TO TERMINATE ONE FILE OF A MULTI-FILE    00008300
REEL.                                                                   00008400
* PAGE UNDERLINE                                                        00008500
GENERAL DISK INFORMATION                                                00008600
* SKIP                                                                  00008700
* 1                                                                     00008800
ANY DISK FILE                                                           00008900
            MAY BE PRINTED THROUGH THIS PROGRAM, BUT ALL FILES ARE      00009000
ARBITRARILY DIVIDED INTO ONE OF TWO CATEGORIES FOR PRINTING             00009100
PURPOSES.  IF A FILE HAS A LOGICAL RECORD LENGTH OF 80 CHARACTERS, IT IS00009200
 ASSUMED TO BE CARD-IMAGES AND IS PRINTED IN AN APPROPRIATE FORMAT      00009300
AS DESCRIBED BRIEFLY BELOW.                                             00009400
ALL OTHER FILES ARE PRINTED IN A STANDARD FORMAT ALSO DESCRIBED BELOW.  00009500
* 1                                                                     00009600
THE ONLY PARAMETERS PASSED TO THE PROGRAM AS DATA (OTHER THAN THOSE     00009700
DESCRIBED UNDER CONTROL INFORMATION) ARE THE FILE NAMES, WHICH MAY BE   00009800
ANY DISK FILE NAME EXCEPT A NAME WHICH CORRESPONDS TO A SPECIAL         00009900
FUNCTION CONTROL AS                                                     00010000
DESCRIBED UNDER CONTROL INSTRUCTIONS.  ALL OTHER INFORMATION, INCLUDING 00010100
RECORD AND BLOCK LENGTHS, IS DERIVED BY THE PROGRAM FROM THE DISK       00010200
DIRECTORY.                                                              00010300
* 1                                                                     00010400
            THE PROGRAM WILL NOT HANDLE A FILE WHOSE LOGICAL RECORD     00010500
LENGTH EXCEEDS 1023 WORDS (8184 CHARACTERS), BUT A SUITABLE MESSAGE     00010600
WILL BE OUTPUT.  IF FOR ANY REASON A RECORD OR BLOCK LENGTH OF ZERO IS  00010700
DETECTED, THE PROGRAM WILL FUDGE BOTH RECORD AND BLOCK LENGTHS  AS 240  00010800
CHARACTERS, OUTPUT A SUITABLE MESSAGE, AND PRINT THE FILE.  BE WARNED   00010900
THAT IN THIS INSTANCE THE DISK DIRECTORY ENTRY WILL BE CHANGED TO       00011000
REFLECT RECORD AND BLOCK LENGTHS OF 240 CHARACTERS BECAUSE OF MCP       00011100
OPERATION.                                                              00011200
* 1                                                                     00011300
WHEN PRINTING A CARD-IMAGE FILE, EACH LINE PRINTED CONTAINS A           00011400
LOGICAL RECORD BOUNDED BY THE SYMBOLS "<" AND ">". EACH LOGICAL RECORD  00011500
NUMBER IS PRINTED AT THE EXTREME LEFT OF THE PRINTLINE.  THE PROGRAM    00011600
EXAMINES COLUMNS 73-80 OF EACH  RECORD, AND IF THESE COLUMNS ARE ALL    00011700
NUMERIC, ASSUMES THAT THIS IS A SEQUENCE NUMBER, AND WILL PRINT         00011800
THIS NUMBER AT THE IMMEDIATE LEFT OF THE RECORD IMAGE TO ENHANCE        00011900
READABILITY.                                                            00012000
* 1                                                                     00012100
WHEN PRINTING A NON-CARD-IMAGE FILE, THE LOGICAL RECORD NUMBER AND THE  00012200
BLOCK NUMBER ARE PRINTED ONCE PER LOGICAL RECORD AND AFTER SKIPPING TO  00012300
A NEW PAGE.  EACH LOGICAL RECORD IS BROKEN INTO AS MANY CHUNKS OF 100   00012400
CHARACTERS AS ARE NECESSARY TO PRINT IT.  EACH LOGICAL RECORD CHUNK     00012500
IS IDENTIFIED UNDER THE COLUMN HEADING "POSITIONS".                     00012600
* 1                                                                     00012700
FOR EACH DISK FILE TO BE PRINTED, THE PROGRAM CHECKS TO SEE IF THE USER 00012800
IS AN AUTHORIZED USER OF THE FILE, ASSUMING THERE IS FILE SECURITY      00012900
ATTACHED TO THIS DISK FILE.  IF UNAUTHORIZED, THE PROGRAM WILL REJECT   00013000
THE REQUEST.                                                            00013100
* PAGE UNDERLINE                                                        00013200
CONTROL INSTRUCTIONS                                                    00013300
* TITLE UNDERLINE                                                       00013400
CONTROL INSTRUCTIONS (CONTD)                                            00013500
* DEFINE 3 = PARAGRAPH - 17, 72, 0 *                                    00013600
* DEFINE 4 = SKIP PARAGRAPH - 17,72,0      *                            00013700
* 4                                                                     00013800
CARD/READER WILL CAUSE THE PROGRAM TO LOOK FOR ITS INPUT, FROM THIS     00013900
     POINT ON, TO BE IN A CARD FILE ("FILES") INSTEAD OF FROM THE SPO.  00014000
     THIS OPTION CANNOT BE REVERSED DURING A RUN.                       00014100
* 4                                                                     00014200
SINGLE/SPACE .                                                          00014300
* 3                                                                     00014400
DOUBLE/SPACE THESE INSTRUCTIONS CONTROL THE SPACING ON THE LINE         00014500
     PRINTER.  DEFAULT IS SINGLE. ONCE EITHER OPTION IS SET, IT WILL    00014600
     BE IN FORCE UNTIL REVERSED BY ENTERING THE ALTERNATE OPTION.       00014700
*4                                                                      00014800
SIX/LINES   .                                                           00014900
* 3                                                                     00015000
EIGHT/LINES REFERS TO THE LINES PER INCH THE PROGRAM IS TO              00015100
CONSIDER EXISTS ON THE FORMS IN THE PRINTER, THE OVERALL DEPTH          00015200
     ALWAYS ASSUMED AS 11 INCHES.  SIX LINES PER INCH IS DEFAULT.       00015300
     ONCE SET, THE OPTION IS HELD UNTIL REVERSED.                       00015400
* 4                                                                     00015500
WIDE/PAPER   .                                                          00015600
*3                                                                      00015700
NARROW/PAPER                                                            00015800
     TELLS THE PROGRAM THAT IT MAY USE EITHER 120 PRINT POSITIONS       00015900
     PER LINE OR 85 PRINT POSITIONS PER LINE.  DEFAULT IS WIDE.  ONCE   00016000
     SET THE OPTION IS HELD UNTIL REVERSED.                             00016100
* DEFINE 5 = SKIP PARAGRAPH 17, 72, 0   *                               00016200
* 5                                                                     00016300
WHEN PRINTING TAPE FILES ON NARROW PAPER,                               00016400
                               IF THE INPUT APPEARS TO BE CARD-IMAGES   00016500
     (I.E. THE BLOCK IS EITHER AN UNLINKED NON-ALGOL-TYPE BLOCK         00016600
     AND IS A MULTIPLE OF 80 CHARACTERS, OR THE BLOCK IS A LINKED       00016700
     ALGOL-TYPE BLOCK IN CARD-IMAGES) THEN THE PRINTOUT IS ADJUSTED SO  00016800
     THAT THE INDICATIVE INFORMATION SUCH AS BLOCK NUMBER,              00016900
     RECORD NUMBER, BLOCK POSITIONS, PARITY, ETC ARE ELIMINATED.        00017000
     FURTHER, IN THIS CASE, THE PRINTING (AND PUNCHING, IF SPECIFIED)   00017100
     OF LINK WORDS IS INHIBITED.                                        00017200
* 5                                                                     00017300
     IF, ON NARROW/PAPER, A NON-CARD-IMAGE TAPE FILE IS BEING PRINTED,  00017400
     THE INDICATIVE BLOCK AND RECORD INFORMATION IS PRINTED ON A LINE   00017500
     PRECEDING THE ACTUAL BLOCK CONTENTS INSTEAD OF ON THE SIDES OF     00017600
     THE BLOCK PRINTOUT AS IS DONE ON WIDE/PAPER.                       00017700
* 5                                                                     00017800
     NOTE THAT ON WIDE PAPER FOR ALL TAPE FILES AND ON NARROW/PAPER     00017900
     FOR NON-CARD-IMAGE TAPE FILES, IF THE BLOCK LOOKS LIKE A LINKED    00018000
     ALGOL-TYPE BLOCK, THE LINK WORDS WILL BE PRINTED (AND PUNCHED IF SO00018100
     SPECIFIED) ON SEPARATE LINES TO ENHANCE READABILITY.               00018200
* 5                                                                     00018300
     NARROW/PAPER CANNOT BE USED ON NON-CARD-IMAGE DISK FILES.          00018400
     IF AN ATTEMPT IS MADE TO DO SO, AN ERROR MESSAGE WILL BE TYPED ON  00018500
     ON THE SPO, AND THE FILE REQUEST WILL BE REJECTED.                 00018600
* 4                                                                     00018700
PUNCH/ON     .                                                          00018800
* 3                                                                     00018900
PUNCH/NEXT   .                                                          00019000
* 3                                                                     00019100
PUNCH/OFF THESE OPTIONS CONTROL PUNCHING OF FILES.                      00019200
     DEFAULT OPTION IS PUNCH/OFF WHICH ELIMINATES PUNCHING.             00019300
     PUNCH/NEXT WILL TURN THE PUNCH ON FOR THE NEXT FILE ONLY.          00019400
     PUNCH/ON WILL TURN THE PUNCH ON UNTIL REVERSED BY A PUNCH/OFF.     00019500
     PUNCHING IS DONE BLINDLY IN GROUPS OF 80 CHARACTERS PER CARD       00019600
      REGARDLESS                                                        00019700
          OF RECORD, FILE, OR BLOCK CHARACTERISTICS (EXCEPT FOR LINK    00019800
  WORDS ON TAPE AS SPECIFIED UNDER NARROW/PAPER).  THERE                00019900
     ARE NO CONTROLS SUCH AS BLOCK NUMBERS ETC PUNCHED.  REFORMATTING   00020000
     OF PUNCHED OUTPUT ONTO ANOTHER MEDIA IS A USER RESPONSIBILITY.     00020100
     NATURALLY, THIS IS NO PROBLEM FOR CARD-IMAGE FILES.                00020200
* 5                                                                     00020300
     REMEMBER TO USE NARROW/PAPER WHEN PUNCHING LINKED ALGOL-TYPE       00020400
     BLOCKED CARD-IMAGE TAPES SUCH AS 0CRDIMG AND SOLT TAPES.           00020500
* 4                                                                     00020600
TAPE/PRINT .                                                            00020700
* 3                                                                     00020800
TAPE/BINARY .                                                           00020900
* 3                                                                     00021000
TAPE/ALPHA .                                                            00021100
* 3                                                                     00021200
TAPE/FILE .                                                             00021300
* 3                                                                     00021400
TAPE/FILEA THESE OPTIONS CONDITION THE PROGRAM FOR TAPE INPUT.          00021500
     TAPE/BINARY AND TAPE/ALPHA WILL CAUSE THE PROGRAM TO ASSUME THE    00021600
     RECORDING MODE OF THE TAPE IS BINARY OR ALPHA (I.E. ODD OR EVEN    00021700
     PARITY). TAPE/PRINT WILL ALLOW THE PROGRAM TO TRY TO DETERMINE     00021800
     ITSELF THE PARITY OF THE TAPE, TRYING BINARY FIRST.                00021900
     TAPE/FILE GIVES THE ABILITY TO SPECIFY TO THE PROGRAM A PARTICULAR 00022000
     (NAMED) TAPE FILE TO BE PROCESSED, WHERE THE TAPE IS BINARY        00022100
     RECORDING MODE.  TAPE/FILEA IS THE SAME AS TAPE/FILE EXCEPT THAT   00022200
     THE TAPE IS EXPECTED TO BE ALPHA RECORDING MODE.  TAPE/FILE AND    00022300
     TAPE/FILEA SHOULD BE USED WHEN THE OPERATOR WANTS THE INPUT        00022400
     TO BE A SPECIFIC FILE ON A MULTI-FILE REEL.                        00022500
* 5                                                                     00022600
     WHEN ANY OF TAPE/PRINT OR TAPE/BINARY OR TAPE/ALPHA OPTIONS ARE    00022700
     ENTERED, THE PROGRAM TYPES OUT                                     00022800
     A MESSAGE "UL THE TAPE FOR DISCAR" AND THEN DOES AN INITIAL        00022900
     READ ON THE TAPE FILE, CAUSING AN MCP "#NO FIL" MESSAGE.           00023000
     THE OPERATOR MUST THEN <MIX>UL THE SPECIFIC TAPE WANTED.           00023100
     IF TAPE/PRINT IS USED, AND THE TAPE IS ALPHA, THERE WILL BE TWO    00023200
     UL REQUESTS AND THE OPERATOR WILL HAVE TO UL THE TAPE TWICE.       00023300
* 5                                                                     00023400
     WHEN TAPE/FILE OR TAPE/FILEA ARE USED, THE PROGRAM TYPES OUT A     00023500
     MESSAGE "TAPE MFID/ID" FOLLOWED BY "FILE PLEASE".  THE PROGRAM THEN00023600
     EXPECTS THE OPERATOR TO ENTER THE <MFID>/<ID> OF THE TAPE TO BE    00023700
     PROCESSED.  HOWEVER, IF THE OPERATOR TYPES IN FORMATTING CONTROLS  00023800
     SUCH AS NARROW/PAPER ETC THEY WILL BE RECOGNIZED AND THE PROGRAM   00023900
     WILL CONTINUE TO LOOK FOR A TAPE FILE NAME.  ONCE THE NAME HAS BEEN00024000
     ENTERED, THE PROGRAM (THRU THE MCP OF COURSE) WILL FIND AND        00024100
     PROCESS THE SPECIFIED FILE WITHOUT FURTHER INTERVENTION.  LABELS   00024200
     AND TAPE MARKS WILL NOT BE PRINTED IN THIS MODE.  NATURALLY, THIS  00024300
     MODE IS MOST USEFUL WHEN WANTING TO PROCESS THE NTH FILE OF        00024400
     A MULTI-FILE REEL.                                                 00024500
* 5                                                                     00024600
     AN EXPLANATION OF PRINT FORMATTING OF TAPE INPUT IS CONTAINED IN   00024700
     WIDE/PAPER NARROW/PAPER.                                           00024800
* 5                                                                     00024900
     WHEN IN TAPE MODE THE PROGRAM SHOULD HANDLE ANY KIND OF TAPE       00025000
     WHETHER SINGLE OR MULTI-FILE REEL, WITHIN THE CONSTRAINT THAT THE  00025100
     TAPE MUST FOLLOW   NORMAL BURROUGHS STANDARDS FOR SUCH TAPES.      00025200
     ON MULTI-FILE REELS EACH FILE WILL START ON A NEW PAGE.            00025300
* 5                                                                     00025400
STANDARD BURROUGHS LABELS FOR B-5500 AND TAPE MARKS ARE RECOGNIZED      00025500
     AND ARE IDENTIFIED DISTINCTIVELY ON THE PRINTOUTS.                 00025600
* 5                                                                     00025700
     PARITY ERRORS ARE DETECTED AND INDICATED IN AN EYE-CATCHING MANNER 00025800
     ON THE PRINTOUT, AND THE PARITY BLOCK IS PRINTED.  NATURALLY, IF   00025900
     TAPE/BINARY OR TAPE/ALPHA IS USED AND THE TAPE IS NOT THE          00026000
     SPECIFIED PARITY, EACH BLOCK WILL INDICATE A PARITY ERROR.         00026100
* 4                                                                     00026200
CARDECK/||||||| CONDITIONS THE PROGRAM TO EXPECT A CARD INPUT FILE WHOSE00026300
     LABEL (IE DATA) ID IS <|||||||>. THIS FUNCTION AUTOMATICALLY       00026400
     INVOKES NARROW/PAPER AND WILL PROCESS THE CARD FILE AS IF IT       00026500
        WERE AN UNBLOCKED CARD IMAGE TAPE.                              00026600
* 4                                                                     00026700
NOMORE      .                                                           00026800
* 3                                                                     00026900
<NULL> IF, IN RESPONSE TO THE PROGRAMS "FILE PLEASE" REQUEST AND        00027000
     SUBSEQUENT SPO OR CARD READ, THE WORD "NOMORE" OR A NULL ENTRY     00027100
     (OR BLANK CARD) IS ENTERED, THE PROGRAM WILL TERMINATE WITH EOJ.   00027200
*QUIT                                                                   00027300
;                                                                       00027400
%...................D I S C A R................................         00027500
BEGIN                                                                   00027600
% DO NOT PLACE ANY DECLARATIONS EXCEPT COMMENTS BETWEEN                 00027700
% THE INITIAL BEGIN & THE DECLARATION "BOOLEAN COMMITSUICIDE;";         00027800
BOOLEAN COMMITSUICIDE;                                                  00027900
FORMAT VERSION ("DISCAR VERSION 70MAY21");                              00028000
INTEGER MAXLINES;DEFINE SIXLINES=50#,EIGHTLINES=70#;                    00028100
DEFINE FORMATFILENAME=PREFIX.[6:36],PREFIX,                             00028200
                      IF PREFIX="DECK   "THEN SUFFIX.[6:30]             00028300
                                         ELSE SUFFIX.[6:36],%WATCHES OUT00028400
                                         %FOR LEFTARROW IN 6TH POSITION 00028500
                                          %OF CONTROL DECK SUFFIX       00028600
                                          %ZND DROPS IT SO SPO          00028700
                                          %MESSAGES DONT TRUNCATE       00028800
                      SUFFIX#;                                          00028900
INTEGER DISKRECORDSIZE,DISKBLOCKSIZE,CHUNKSOF100,LESSTHAN100;           00029000
INTEGER CHUNKCOUNT,BLOCKNUMBER,RECORDNUMBER,RECORDSPERBLOCK;            00029100
FILE SPO 11(5,10);                                                      00029200
FILE IN CARDRDR "FILES" (1,10);                                         00029300
FILE PRINT 18(2,15);                                                    00029400
SWITCH FILE SOURCE:=SPO,PRINT;                                          00029500
FILE DISKIN DISK SERIAL"ANYOLD""DISKFIL"(2,0,0);                        00029600
ALPHA FILE ALPHATAPE 9(1,1023);FILE BINARYTAPE 9(1,1023);               00029700
SWITCH FILE ANYTAPE:=ALPHATAPE,BINARYTAPE;                              00029800
INTEGER ALPHAORBINARYTAPE;                                              00029900
INTEGER NUMBEROFLINKWORDS;                                              00030000
INTEGER THERECORDSIZE,NOOFTAPELINES,J;                                  00030100
    INTEGER FILENO,BLOCKNO;                                             00030200
    INTEGER RECORDCOUNT,PARITYCOUNT;                                    00030300
         BOOLEAN LABELEXPECT;                                           00030400
    INTEGER THEBINYR;                                                   00030500
    BOOLEAN ITSOVER,LABELREAD;                                          00030600
    BOOLEAN FIRSTREAD,PREREADGAVETPMK;                                  00030700
BOOLEAN INBLOCK;                                                        00030800
    ALPHA PARITY,OUTSWITCH,FILEID,MFID;                                 00030900
    ALPHA LABSWITCH;                                                    00031000
    ALPHA TLABEL;                                                       00031100
     ARRAY PRINTLINE [1:10];                                            00031200
FILE DIRECTORY  DISK RANDOM"DIRCTRY""DISK   "(5,30);                    00031300
FILE PUNCH 0(2,10);                                                     00031400
FORMAT SUICIDE("XXXXXXXXXX OPERATOR HAS KILLED THIS FILE "              00031500
               "BEFORE COMPLETION XXXXXXXXXXXXXXXXXXXX");               00031600
ARRAY DISKIMAGE[0:1022];                                                00031700
ARRAY CARDIMAGE[0:9];                                                   00031800
ARRAY PRINTIMAGE[0:15];                                                 00031900
ARRAY SEARCHREPLY[0:6];                                                 00032000
ARRAY SPIN[0:9];                                                        00032100
ARRAY DIRECORD,DIRFILEHDR[0:29];                                        00032200
ARRAY MONTHNAMES[0:11],MONTHDAYS[0:11];                                 00032300
FORMAT SPECTAPENAMEPLS("TAPE MFID/ID");                                 00032400
FORMAT WHATFILE("FILE PLEASE");                                         00032500
FORMAT SUFXERR("YOU LEFT OUT THE SLASH DUMMY, TRY AGAIN.");             00032600
FORMAT NEXTFILE("NEXT FILE IS ",A6,A1,"/",A6,A1);                       00032700
FORMAT BYPASSOBJECTFILE("FILE ",A6,A1,"/",A6,A1," BYPASSED ",           00032800
                          "(OBJECT CODE)");                             00032900
FORMAT NULLFILEGROUP("SORRY, NULL FILE GROUP ",A6,A1,"/",A6,A1);        00033000
FORMAT TAPEFILE("UL THE TAPE FOR DISCAR");                              00033100
BOOLEAN SPECIFICTAPE;                                                   00033200
BOOLEAN CARDECK;                                                        00033300
BOOLEAN REMEMBERNARROWPAPER;                                            00033400
BOOLEAN NARROWPAPER;                                                    00033500
BOOLEAN BLOCKISCARDIMAGES;                                              00033600
BOOLEAN NULLFILES;                                                      00033700
LABEL GETANOTHER,STOPRUN,FREAKOUT;                                      00033800
LABEL NOTTAPEINPUT;                                                     00033900
ALPHA PREFIX,SUFFIX,FROMHERE,TODAYSDATE;                                00034000
INTEGER ERROR,WHICHSOURCE,DIRECKEY,POINTER,CURRENTIME,THEDAY;           00034100
INTEGER THEMONTH,LINECOUNT,PAGENUMBER;                                  00034200
BOOLEAN PUNCHIT,ALLSUFF,ALLPREF,SEARCHDIR,HOLDPUNCH,SINGLESPACE;        00034300
STREAM PROCEDURE COMPILEID(INPUT,PREFIX,SUFFIX,ERROR);                  00034400
BEGIN                                                                   00034500
LABEL PRELOOP,SUFLOOP,EGZIT,GETSLASH,GOTSLASH;                          00034600
LOCAL LEFTARROW,QUESTIONMARKS,HOLDINX;                                  00034700
DI:=LOC LEFTARROW;DS:=43RESET;DS:=5SET;%MAKE LEFTARROW                  00034800
DI:=LOC QUESTIONMARKS;DS:=6RESET;7(DS:=2RESET;DS:=2SET;DS:=2RESET);     00034900
SI:=LOC LEFTARROW;SI:=SI+7;DI:=INPUT;DI:=DI+63;DI:=DI+8;DS:=1CHR;       00035000
% MAKE SURE LEFTARROW IN COL 73 IN CASE INPUT IS CARD AND NOT SPO       00035100
     SI:=INPUT;                                                         00035200
     DI:=PREFIX;DS:=8LIT"0       ";                                     00035300
     DI:=SUFFIX;DS:=8LIT"0       ";                                     00035400
     DI:=PREFIX;DI:=DI+1;                                               00035500
   7(                                                                   00035600
PRELOOP:IF SC=" "THEN BEGIN SI:=SI+1;GO TO PRELOOP END;                 00035700
        IF SC="/"THEN JUMP OUT TO GETSLASH;                             00035800
        HOLDINX:=DI;DI:=LOC LEFTARROW;DI:=DI+7;                         00035900
        IF SC=DC                                                        00036000
        THEN BEGIN                                                      00036100
             SI:=LOC QUESTIONMARKS;                                     00036200
             DI:=SUFFIX;DS:=1WDS;%QU MRKS;                              00036300
             DI:=ERROR;DI:=DI+7;DS:=1LIT"1";                            00036400
             JUMP OUT TO EGZIT                                          00036500
             END;                                                       00036600
        SI:=SI-1;DI:=HOLDINX;                                           00036700
        DS:=1CHR);                                                      00036800
GETSLASH:IF SC="/"                                                      00036900
         THEN BEGIN                                                     00037000
              SI:=SI+1;                                                 00037100
              DI:=SUFFIX;DI:=DI+1;                                      00037200
              GO TO GOTSLASH                                            00037300
              END;                                                      00037400
         HOLDINX:=DI;DI:=LOC LEFTARROW;DI:=DI+7;                        00037500
         IF SC=DC                                                       00037600
         THEN BEGIN                                                     00037700
              SI:=LOC QUESTIONMARKS;                                    00037800
              DI:=SUFFIX;DS:=1WDS;% QUESTN MKS                          00037900
              DI:=ERROR;DI:=DI+7;DS:=1LIT"2";                           00038000
              GO TO EGZIT;                                              00038100
              END                                                       00038200
         ELSE DI:=HOLDINX;                                              00038300
         GO TO GETSLASH;                                                00038400
GOTSLASH:                                                               00038500
   7(                                                                   00038600
SUFLOOP:HOLDINX:=DI;DI:=LOC LEFTARROW;DI:=DI+7;                         00038700
        IF SC=DC THEN JUMP OUT TO EGZIT;                                00038800
        SI:=SI-1;DI:=HOLDINX;                                           00038900
        IF SC=" "THEN BEGIN SI:=SI+1;GO TO SUFLOOP END;                 00039000
        DS:=1CHR);                                                      00039100
EGZIT:                                                                  00039200
END OF COMPILEID;                                                       00039300
PROCEDURE SETUPDATEANDTIME;                                             00039400
BEGIN                                                                   00039500
%THIS PROCEDURE ONLY ACCESSES THE SYSTEM DATE AND TIME AND              00039600
%STORES THEM,CONVERTING THE DATE INTO A FORM                            00039700
%USABLE BY THE PAGE HEADING WRITE ROUTINE (I.E. ORIGINAL DATE           00039800
% IS YYDDD, & IS CONVERTED TO YY LEFT IN TODAYSDATE, DD IN              00039900
% THEDAY, AND MM IN THEMONTH).                                          00040000
     TODAYSDATE:=TIME(0);CURRENTIME:=TIME(1);                           00040100
              THEBINYR~ TODAYSDATE.[18:6] |10 +TODAYSDATE.[24:6] ;      00040200
         MONTHDAYS[1] ~ REAL(THEBINYR MOD 4 = 0) +28;                   00040300
          %CHECK FOR LEAP YR & ADJUST FEBRUARY DAYS ACCORDINGLY         00040400
     THEDAY:=TODAYSDATE.[30:6]|100+TODAYSDATE.[36:6]|10+                00040500
             TODAYSDATE.[42:6];                                         00040600
     FOR THEMONTH:=0 STEP 1 WHILE THEDAY GTR MONTHDAYS[THEMONTH]        00040700
     DO THEDAY:=THEDAY-MONTHDAYS[THEMONTH];                             00040800
END SETUPDATEANDTIME;                                                   00040900
PROCEDURE TOPOFPAGE;                                                    00041000
BEGIN                                                                   00041100
FORMAT NARROWPAGETOP(X27"LISTING OF FILE "A6,A1"/"A6,A1//               00041200
                     X29"TAKEN AT "I2":"I2" ON "A2,A3,I2,X15"PAGE "I3/  00041300
                     I4" CHARACTER ("I4" WORD) RECORDS BLOCKED "I3" IN "00041400
                     I4" CHAR ("I4" WORD) BLOCKS");                     00041500
FORMAT PAGETOP(X44"LISTING OF FILE "A6,A1"/"A6,A1//                     00041600
               X46"TAKEN AT "I2":"I2                                    00041700
               " ON "A2,A3,I2,X35"PAGE "I3/                             00041800
               I4," CHARACTER (",I4," WORD) RECORDS BLOCKED ",I3,       00041900
               " IN ",I4," CHARACTER (",I4," WORD, ",                   00042000
               I2" SEGMENT) BLOCKS");                                   00042100
FORMAT HUNDREDHEAD(X20,"0",X8,"1",X9,"2",X9,"3",X9,"4",X9,"5",X9,       00042200
                   "6",X9,"7",X9,"8",X9,"9",X9,"+", /                   00042300
                   "BLK RECD  POSITIONS ",10("1234567890") / );         00042400
FORMAT CARDHEAD("    RECD      SEQ   0"                                 00042500
                    X8"1"X9"2"X9"3"X9                                   00042600
                "4",X9,"5",X9,"6",X9,"7",X9,"8",  /                     00042700
                X20,8("1234567890") / );                                00042800
FORMAT NARROWCARDHEAD(X1,                                               00042900
                      "0"X8"1"X9"2"X9"3"X9"4"X9"5"X9"6"X9"7"X9"8",/     00043000
            X1,    8("1234567890")/);                                   00043100
FORMAT PUNCHTOP("PUNCHOUT OF FILE ",A6,A1,"/",A6,A1,                    00043200
                " TAKEN AT ",I2,":",I2," ON ",A2,A3,I2);                00043300
     WRITE(PRINT[PAGE]);                                                00043400
     IF NOT NARROWPAPER                                                 00043500
     THEN WRITE(PRINT,PAGETOP,                                          00043600
           PREFIX.[6:36],PREFIX,SUFFIX.[6:36],SUFFIX,                   00043700
           CURRENTIME DIV 216000,CURRENTIME MOD 216000 DIV 3600,        00043800
           TODAYSDATE.[18:12],MONTHNAMES[THEMONTH],THEDAY,              00043900
           PAGENUMBER:=PAGENUMBER+1,                                    00044000
           DISKRECORDSIZE|8,DISKRECORDSIZE,RECORDSPERBLOCK,             00044100
           DISKBLOCKSIZE|8,DISKBLOCKSIZE,                               00044200
           DISKBLOCKSIZE DIV 30 +                                       00044300
               REAL(DISKBLOCKSIZE MOD 30 GTR 0))                        00044400
     ELSE WRITE(PRINT,NARROWPAGETOP,                                    00044500
          PREFIX.[6:36],PREFIX,SUFFIX.[6:36],SUFFIX,                    00044600
               CURRENTIME DIV 216000,CURRENTIME MOD 216000 DIV 3600,    00044700
          TODAYSDATE.[18:12],MONTHNAMES[THEMONTH],THEDAY ,              00044800
               PAGENUMBER:=PAGENUMBER+1,                                00044900
          DISKRECORDSIZE|8,DISKRECORDSIZE,RECORDSPERBLOCK,              00045000
          DISKBLOCKSIZE|8,DISKBLOCKSIZE);                               00045100
     IF PUNCHIT AND PAGENUMBER=1                                        00045200
     THEN WRITE(PUNCH,PUNCHTOP,                                         00045300
                PREFIX.[6:36],PREFIX,SUFFIX.[6:36],SUFFIX,              00045400
                CURRENTIME DIV 216000,CURRENTIME MOD 216000 DIV         00045500
                      3600,TODAYSDATE.[18:12],                          00045600
                MONTHNAMES[THEMONTH],THEDAY);                           00045700
           LINECOUNT:=0;                                                00045800
     IF DISKRECORDSIZE NEQ 10 AND NOT NARROWPAPER                       00045900
                  THEN WRITE(PRINT,HUNDREDHEAD)                         00046000
          ELSE IF NOT NARROWPAPER THEN WRITE(PRINT,CARDHEAD)            00046100
              ELSE WRITE(PRINT,NARROWCARDHEAD);                         00046200
END OF TOPOFPAGE;                                                       00046300
PROCEDURE STARTTHEPROGRAM;                                              00046400
BEGIN                                                                   00046500
 WRITE(SPO,VERSION);                                                    00046600
     SINGLESPACE:=TRUE ;                                                00046700
     MAXLINES:=SIXLINES;NARROWPAPER:=FALSE;                             00046800
     FILL MONTHNAMES[*]WITH"JAN","FEB","MAR","APR","MAY","JUN",         00046900
                           "JUL","AUG","SEP","OCT","NOV","DEC";         00047000
     FILL MONTHDAYS[*]WITH 31,28,31,30,31,30,31,31,30,31,30,31;         00047100
END OF START THE PROGRAM;                                               00047200
STREAM PROCEDURE CARDIMAGEPRINTSETUP(CARDIMAGE,PRINTIMAGE,              00047300
                             LINENUMBER,WIDEPAPER);                     00047400
VALUE LINENUMBER,WIDEPAPER;                                             00047500
BEGIN                                                                   00047600
LABEL NOSEQUENCE;                                                       00047700
     DI:=PRINTIMAGE;DS:=8LIT" "; % BLANK OUT THE                        00047800
     SI:=PRINTIMAGE;DS:=14WDS;   %       PRINTLINE                      00047900
     SI:=CARDIMAGE;DI:=PRINTIMAGE;                                      00048000
     WIDEPAPER(DI:=DI+19); DI:=DI+1;                                    00048100
     2(DS:=40CHR); % PLACE CARDIMAGE IN PRINT POSITIONS 21-100          00048200
     DI:=PRINTIMAGE;                                                    00048300
     2(DI:=DI+40);DI:=DI+1;WIDEPAPER(DI:=DI+19);                        00048400
     DS:=LIT">";                                                        00048500
     DI:=PRINTIMAGE;WIDEPAPER(DI:=DI+19);DS:=LIT"<";                    00048600
     SI:=CARDIMAGE;2(SI:=SI+36);                                        00048700
     WIDEPAPER(                                                         00048800
     8(IF SC LSS"0"THEN JUMP OUT TO NOSEQUENCE;%IF THERE IS A           00048900
       IF SC GTR"9"THEN JUMP OUT TO NOSEQUENCE;%SEQUENCE NUMBER         00049000
       SI:=SI+1); % IN THE CARD, PRINT THE NUMBER TO THE LEFT           00049100
                  % OF THE LEFT COLON                                   00049200
     SI:=SI-8;DI:=PRINTIMAGE;DI:=DI+9 ;DS:=8CHR;DI:=DI-8;DS:=8FILL;     00049300
NOSEQUENCE:                                                             00049400
     SI:=LOC LINENUMBER;DI:=PRINTIMAGE;                                 00049500
     DS:=8DEC;DI:=DI-8;DS:=8FILL;%INSERT LINE NUMBER                    00049600
     );                                                                 00049700
END OF CARDIMAGEPRINTSETUP;                                             00049800
STREAM PROCEDURE NOTCARDPRINTSETUP                                      00049900
     (INPUTARRAY,PRINTIMAGE,CHUNKSDIV2,CHUNKSMOD2,                      00050000
      CHARSDIV2,BLOCKNUMBEROR0,                                         00050100
      RECORDNUMBEROR0,FROMCHAR,TOCHAR);                                 00050200
VALUE CHARSDIV2,BLOCKNUMBEROR0,RECORDNUMBEROR0,                         00050300
     FROMCHAR,TOCHAR,CHUNKSDIV2,CHUNKSMOD2;                             00050400
BEGIN                                                                   00050500
     DI:=PRINTIMAGE;DS:=8LIT" ";SI:=PRINTIMAGE;DS:=14WDS;               00050600
             % BLANK OUT PRINTLINE                                      00050700
     SI:=LOC BLOCKNUMBEROR0;DI:=PRINTIMAGE;                             00050800
     DS:=3DEC;DI:=DI-3;DS:=3FILL;%INSERT BLOCK NUMBER                   00050900
     SI:=LOC RECORDNUMBEROR0;DI:=PRINTIMAGE;DI:=DI+4;                   00051000
     DS:=4DEC;DI:=DI-4;DS:=4FILL;%INSERT RECORD NUMBER                  00051100
     SI:=LOC FROMCHAR;DI:=PRINTIMAGE;DI:=DI+9;                          00051200
     DS:=4DEC;DI:=DI-4;DS:=4FILL;%INSERT FROM CHAR                      00051300
     DI:=PRINTIMAGE;DI:=DI+14;DS:=1LIT"-";                              00051400
     SI:=LOC TOCHAR;                                                    00051500
     DS:=4DEC;DI:=DI-4;DS:=4FILL;% INSERT TO CHAR                       00051600
     DI:=PRINTIMAGE;DI:=DI+20;                                          00051700
     SI:=INPUTARRAY;                                                    00051800
     2(2(CHUNKSDIV2(SI:=SI+50)));%  INDENT INTO INPUT BY                00051900
     CHUNKSMOD2(2(SI:=SI+50));   %    NUMBER OF CHUNKS                  00052000
     2(CHARSDIV2(DS:=1CHR));%  FILL IN PRINTLINE WITH DATA              00052100
END OF NOTCARDPRINTSETUP;                                               00052200
STREAM PROCEDURE MAKEACARD(WORDSINCARD,INPUTARRAY,CARDIMAGE);           00052300
VALUE WORDSINCARD;                                                      00052400
BEGIN                                                                   00052500
     DI:=CARDIMAGE;DS:=8LIT" ";SI:=CARDIMAGE;DS:=7WDS;                  00052600
     SI:=INPUTARRAY;DI:=CARDIMAGE;                                      00052700
     WORDSINCARD(DS:=1WDS);                                             00052800
END OF MAKEACARD;                                                       00052900
PROCEDURE FETCHANDANALYZE;                                              00053000
BEGIN                                                                   00053100
LABEL DOITAGIN,NOTSPACE,NOTLINES,NOTPAPER;                              00053200
DOITAGIN:                                                               00053300
  IF WHICHSOURCE = 0 THEN BEGIN                                         00053400
     WRITE(SOURCE[WHICHSOURCE],WHATFILE);READ(SPO,10,SPIN[*]);          00053500
    END ELSE READ (CARDRDR,10,SPIN[*]);                                 00053600
     COMPILEID(SPIN[0],PREFIX,SUFFIX,ERROR);                            00053700
      IF PREFIX="NOMORE " OR PREFIX="       " THEN GO TO STOPRUN;       00053800
     IF ERROR=1 OR ERROR=2                                              00053900
     THEN BEGIN                                                         00054000
          WRITE(SOURCE[WHICHSOURCE],SUFXERR);                           00054100
          ERROR:=0;                                                     00054200
          GO TO DOITAGIN                                                00054300
          END;                                                          00054400
     IF SUFFIX="SPACE  "                                                00054500
     THEN BEGIN                                                         00054600
          IF PREFIX="SINGLE "                                           00054700
          THEN SINGLESPACE:=TRUE                                        00054800
          ELSE                                                          00054900
          IF PREFIX="DOUBLE "                                           00055000
          THEN SINGLESPACE:=FALSE                                       00055100
          ELSE GO TO NOTSPACE;                                          00055200
          GO TO DOITAGIN;                                               00055300
          END;                                                          00055400
NOTSPACE:                                                               00055500
     IF PREFIX="PUNCH  "                                                00055600
     THEN BEGIN                                                         00055700
          IF SUFFIX="NEXT   "                                           00055800
          THEN BEGIN                                                    00055900
               PUNCHIT:=TRUE;                                           00056000
               HOLDPUNCH:=FALSE                                         00056100
               END                                                      00056200
          ELSE                                                          00056300
          IF SUFFIX="ON     "THEN PUNCHIT:=HOLDPUNCH:=TRUE              00056400
          ELSE                                                          00056500
          IF SUFFIX="OFF    "THEN PUNCHIT:=HOLDPUNCH:=FALSE;            00056600
      IF PREFIX="PUNCH  "AND(SUFFIX="NEXT   "OR SUFFIX="ON     " OR     00056700
             SUFFIX="OFF    ")                                          00056800
      THEN GO TO DOITAGIN;                                              00056900
          END;%OF PREFIX IS PUNCH                                       00057000
     IF SUFFIX="LINES  "                                                00057100
     THEN BEGIN                                                         00057200
         IF PREFIX="SIX    "                                            00057300
         THEN MAXLINES:=SIXLINES                                        00057400
         ELSE                                                           00057500
         IF PREFIX="EIGHT  "                                            00057600
     THEN MAXLINES:=EIGHTLINES                                          00057700
         ELSE GO TO NOTLINES;                                           00057800
         GO TO DOITAGIN;                                                00057900
         END OF SUFFIX LINES PER PAGE;                                  00058000
NOTLINES:                                                               00058100
    IF SUFFIX="PAPER  "                                                 00058200
    THEN BEGIN                                                          00058300
         IF PREFIX="NARROW "                                            00058400
         THEN NARROWPAPER:=TRUE                                         00058500
         ELSE                                                           00058600
         IF PREFIX="WIDE   "                                            00058700
         THEN NARROWPAPER:=FALSE                                        00058800
         ELSE GO TO NOTPAPER;                                           00058900
         GO TO DOITAGIN;                                                00059000
         END OF PAPER WIDTH;                                            00059100
NOTPAPER:                                                               00059200
    IF PREFIX="CARD   "AND SUFFIX="READER "                             00059300
    THEN BEGIN WHICHSOURCE:=1;GO TO DOITAGIN END;                       00059400
END OF FETCHANDANALYZE;                                                 00059500
PROCEDURE DOTHERESTOFYOURTHING;FORWARD;                                 00059600
PROCEDURE DOYOURTHING;                                                  00059700
BEGIN                                                                   00059800
LABEL MYTHINGDONE;                                                      00059900
FORMAT NOFILE("SORRY, FILE ",A6,A1,"/",A6,A1," IS NOT ON DISK.");       00060000
FORMAT NOTYOURS("SORRY, FILE ",A6,A1,"/",A6,A1," IS A NO-NO ",          00060100
                "U R NOT AN AUTHORIZED USER.");                         00060200
FORMAT YOUGOTTABEKIDDIN("SORRY, CANNOT HANDLE ",A6,A1,"/",A6,A1,        00060300
                    ", RECORD SIZE GTR 1023 WORDS - WOW");              00060400
FORMAT NOSIZE("FILE ",A6,A1,"/",A6,A1," MISSING RECORD/BLOCK SIZE "     00060500
               "R=",I4," B=",I4, ", ASSUME BOTH 240");                  00060600
FORMAT TOOWIDE("FILE "A6,A1"/"A6,A1" CANNOT USE NARROW/PAPER OPTION");  00060700
     FILL DISKIN WITH PREFIX,SUFFIX;                                    00060800
     SEARCH(DISKIN,SEARCHREPLY[*]);                                     00060900
     IF SEARCHREPLY[0]=-1                                               00061000
     THEN BEGIN                                                         00061100
          WRITE(SOURCE[WHICHSOURCE],NOFILE,FORMATFILENAME);             00061200
          GO TO MYTHINGDONE                                             00061300
          END;                                                          00061400
     IF SEARCHREPLY[0]=0                                                00061500
     THEN BEGIN                                                         00061600
          WRITE(SOURCE[WHICHSOURCE],NOTYOURS,FORMATFILENAME);           00061700
          GO TO MYTHINGDONE                                             00061800
          END;                                                          00061900
     IF DISKRECORDSIZE:=SEARCHREPLY[3]GTR 1023                          00062000
     THEN BEGIN                                                         00062100
          WRITE(SOURCE[WHICHSOURCE],YOUGOTTABEKIDDIN,                   00062200
                FORMATFILENAME);                                        00062300
          GO TO MYTHINGDONE;                                            00062400
          END;                                                          00062500
    IF DISKRECORDSIZE>10 AND NARROWPAPER                                00062600
    THEN BEGIN                                                          00062700
         WRITE(SOURCE[WHICHSOURCE],TOOWIDE,FORMATFILENAME);             00062800
         GO TO MYTHINGDONE                                              00062900
         END;                                                           00063000
     DISKBLOCKSIZE:=SEARCHREPLY[4];                                     00063100
     IF DISKRECORDSIZE=0 OR DISKBLOCKSIZE=0                             00063200
     THEN BEGIN                                                         00063300
          WRITE(SOURCE[WHICHSOURCE],NOSIZE,FORMATFILENAME,              00063400
                DISKRECORDSIZE,DISKBLOCKSIZE);                          00063500
          WRITE(PRINT,NOSIZE,FORMATFILENAME,DISKRECORDSIZE,DISKBLOCKSIZE00063600
                );                                                      00063700
          DISKRECORDSIZE:=DISKBLOCKSIZE:=30;                            00063800
          END;                                                          00063900
     RECORDSPERBLOCK:=DISKBLOCKSIZE DIV DISKRECORDSIZE;                 00064000
     CHUNKSOF100:=(DISKRECORDSIZE|8)DIV 100;                            00064100
     LESSTHAN100:=(DISKRECORDSIZE|8)MOD 100;                            00064200
     BLOCKNUMBER:=RECORDNUMBER:=0;                                      00064300
     SETUPDATEANDTIME;                                                  00064400
     TOPOFPAGE;                                                         00064500
     DOTHERESTOFYOURTHING;                                              00064600
MYTHINGDONE:                                                            00064700
END; % OF DOYOURTHING(PART I)                                           00064800
PROCEDURE DOTHERESTOFYOURTHING;                                         00064900
BEGIN                                                                   00065000
LABEL FILEND;                                                           00065100
FILE DISKIN DISK SERIAL"ANYOLD""DISKFIL"(2,DISKRECORDSIZE,              00065200
                                           DISKBLOCKSIZE);              00065300
ARRAY DISKIMAGE[0:DISKRECORDSIZE];                                      00065400
     FILL DISKIN WITH PREFIX,SUFFIX;                                    00065500
     WHILE TRUE DO                                                      00065600
     BEGIN                                                              00065700
     READ(DISKIN,DISKRECORDSIZE,DISKIMAGE[*])[FILEND];                  00065800
     BLOCKNUMBER:=IF(RECORDNUMBER:=RECORDNUMBER+1)MOD  RECORDSPERBLOCK=100065900
                  OR RECORDSPERBLOCK=1                                  00066000
                  THEN BLOCKNUMBER+1                                    00066100
                  ELSE BLOCKNUMBER;                                     00066200
     IF DISKRECORDSIZE NEQ 10                                           00066300
         AND NOT NARROWPAPER                                            00066400
     THEN BEGIN % ******RECORDS ARE NOT CARD IMAGES                     00066500
          FOR CHUNKCOUNT:=0 STEP 1 UNTIL CHUNKSOF100                    00066600
          DO   BEGIN                                                    00066700
               IF LINECOUNT GEQ MAXLINES THEN TOPOFPAGE;                00066800
               NOTCARDPRINTSETUP(DISKIMAGE[*],PRINTIMAGE[*],            00066900
                                 CHUNKCOUNT DIV 2,                      00067000
                                 IF CHUNKCOUNT MOD 2=1 THEN 1 ELSE 0,   00067100
                                 IF CHUNKCOUNT LSS CHUNKSOF100          00067200
                                 THEN 100 DIV 2                         00067300
                                 ELSE LESSTHAN100 DIV 2,                00067400
                                 IF CHUNKCOUNT=0 OR LINECOUNT=0         00067500
                                 THEN BLOCKNUMBER                       00067600
                                 ELSE 0,                                00067700
                                 IF CHUNKCOUNT=0 OR LINECOUNT=0         00067800
                                 THEN RECORDNUMBER                      00067900
                                 ELSE 0,                                00068000
                                 (CHUNKCOUNT|100)+1,                    00068100
                                 IF CHUNKCOUNT LSS CHUNKSOF100          00068200
                                 THEN(CHUNKCOUNT+1)|100                 00068300
                                 ELSE(CHUNKCOUNT|100)+LESSTHAN100);     00068400
               IF SINGLESPACE                                           00068500
               THEN WRITE(PRINT,15,PRINTIMAGE[*])                       00068600
               ELSE WRITE(PRINT[DBL],15,PRINTIMAGE[*]);                 00068700
               LINECOUNT:=LINECOUNT+(IF SINGLESPACE THEN 1 ELSE 2);     00068800
               END;                                                     00068900
          IF LINECOUNT GEQ MAXLINES                                     00069000
          THEN TOPOFPAGE                                                00069100
          ELSE BEGIN                                                    00069200
               IF SINGLESPACE THEN WRITE(PRINT)ELSE WRITE(PRINT[DBL]);  00069300
               LINECOUNT:=LINECOUNT+(IF SINGLESPACE THEN 1 ELSE 2);     00069400
               END;                                                     00069500
          IF PUNCHIT                                                    00069600
          THEN FOR POINTER:=0 STEP 10 UNTIL DISKRECORDSIZE-1            00069700
               DO   BEGIN                                               00069800
                    MAKEACARD(IF POINTER+9 LSS DISKRECORDSIZE           00069900
                              THEN 10                                   00070000
                              ELSE DISKRECORDSIZE MOD 10,               00070100
                              DISKIMAGE[POINTER],                       00070200
                              CARDIMAGE[*]);                            00070300
                    WRITE(PUNCH,10,CARDIMAGE[*])                        00070400
                    END;                                                00070500
     IF COMMITSUICIDE                                                   00070600
     THEN BEGIN                                                         00070700
          COMMITSUICIDE:=FALSE;                                         00070800
          WRITE(PRINT,SUICIDE);                                         00070900
          IF PUNCHIT THEN WRITE(PUNCH,SUICIDE);                         00071000
          GO TO FILEND                                                  00071100
          END;                                                          00071200
          END   % OF RECORDS NOT CARD IMAGES                            00071300
     ELSE BEGIN % ::::: RECORDS ARE CARD IMAGES                         00071400
          IF LINECOUNT GEQ MAXLINES THEN TOPOFPAGE;                     00071500
          CARDIMAGEPRINTSETUP(DISKIMAGE[*],PRINTIMAGE[*],               00071600
     RECORDNUMBER,IF NARROWPAPER THEN 0 ELSE 1);                        00071700
          IF SINGLESPACE                                                00071800
          THEN WRITE(PRINT,15,PRINTIMAGE[*])                            00071900
          ELSE WRITE(PRINT[DBL],15,PRINTIMAGE[*]);                      00072000
          IF SINGLESPACE THEN LINECOUNT:=LINECOUNT+1                    00072100
                              ELSE LINECOUNT:=LINECOUNT+2;              00072200
          IF PUNCHIT THEN WRITE(PUNCH,10,DISKIMAGE[*]);                 00072300
          IF COMMITSUICIDE                                              00072400
          THEN BEGIN                                                    00072500
               COMMITSUICIDE:=FALSE;                                    00072600
               WRITE(PRINT,SUICIDE);                                    00072700
               IF PUNCHIT THEN WRITE(PUNCH,SUICIDE);                    00072800
               GO TO FILEND                                             00072900
               END;                                                     00073000
         END;                                                           00073100
     END OF WHILE TRUE LOOP;                                            00073200
FILEND:                                                                 00073300
     IF PUNCHIT AND NOT HOLDPUNCH                                       00073400
     THEN BEGIN                                                         00073500
          PUNCHIT:=FALSE;                                               00073600
          CLOSE(PUNCH,RELEASE)                                          00073700
          END;                                                          00073800
     WRITE(PRINT[PAGE]);                                                00073900
     PAGENUMBER:=RECORDNUMBER:=0;                                       00074000
     CLOSE(DISKIN,RELEASE);                                             00074100
END OF DOYOURTHING;                                                     00074200
    PROCEDURE TOPOFTAPEPAGE;                                            00074300
    BEGIN                                                               00074400
FORMAT HEADING(X44,"LISTING OF FILE "A6,A1,"/"A6,A1//                   00074500
               X46"TAKEN AT "I2":"I2,X1"ON "A2,X1,A3,X1,I2,             00074600
              X35,"PAGE",X2,I3//                                        00074700
              "FILE BLOCK TYPE   POSITION   0",                         00074800
              X8,"1",X9,"2",X9,"3",X9,"4",X9,"5",X9,"6",X9,"7",         00074900
              X9,"8",X2,"MODE",/,                                       00075000
              "NO     NO  ",X18,8("1234567890")/);                      00075100
FORMAT NARROWHEADING(X27"LISTING OF FILE "A6,A1"/"A6,A1//               00075200
                     X29"TAKEN AT "I2":"I2" ON "A2,X1,A3,X1,I2,         00075300
                           X15"PAGE "I3//                               00075400
                     X1"0"X8"1"X9"2"X9"3"X9"4"X9"5"X9"6"X9"7"X9"8"/     00075500
                     X1,8("1234567890")/);                              00075600
FORMAT PUNCHHEADING("PUNCHOUT OF FILE "A6,A1,"/"A6,A1" TAKEN AT "       00075700
     I2":"I2" ON "A2,A3,I2);                                            00075800
              WRITE(PRINT[PAGE]);                                       00075900
     IF NARROWPAPER                                                     00076000
     THEN WRITE(PRINT,NARROWHEADING,                                    00076100
                MFID.[6:36],MFID,FILEID.[6:36],FILEID,                  00076200
                CURRENTIME DIV 216000,CURRENTIME MOD 216000 DIV 3600,   00076300
                TODAYSDATE.[18:12],MONTHNAMES[THEMONTH],THEDAY,         00076400
                PAGENUMBER:=PAGENUMBER+1)                               00076500
     ELSE                                                               00076600
    WRITE(PRINT,HEADING,MFID.[6:36],MFID,FILEID.[6:36],FILEID,          00076700
              CURRENTIME DIV 216000,CURRENTIME MOD 216000 DIV 3600,     00076800
              TODAYSDATE.[18:12],MONTHNAMES[THEMONTH],THEDAY,           00076900
              PAGENUMBER ~ PAGENUMBER +1);                              00077000
         IF PUNCHIT AND PAGENUMBER =1                                   00077100
         THEN WRITE(PUNCH,PUNCHHEADING,MFID.[6:36],MFID,                00077200
         FILEID.[6:36],FILEID,CURRENTIME DIV 216000,                    00077300
                    CURRENTIME MOD 216000 DIV 3600,                     00077400
              TODAYSDATE.[8:12],MONTHNAMES[THEMONTH],THEDAY);           00077500
         LINECOUNT ~ 0;                                                 00077600
         OUTSWITCH~ "       ";                                          00077700
    END OF TOPOFTAPEPAGE;                                               00077800
    PROCEDURE FINISHUP;                                                 00077900
         BEGIN                                                          00078000
         IF HOLDPUNCH THEN PUNCHIT:=TRUE;%IN CASE SET FALSE BY TPMARK   00078100
     IF COMMITSUICIDE                                                   00078200
     THEN BEGIN                                                         00078300
          COMMITSUICIDE:=FALSE;                                         00078400
          WRITE(PRINT,SUICIDE);                                         00078500
          IF PUNCHIT THEN WRITE(PUNCH,SUICIDE);                         00078600
          END;                                                          00078700
              IF PUNCHIT AND NOT HOLDPUNCH THEN                         00078800
              BEGIN PUNCHIT~FALSE; CLOSE(PUNCH,RELEASE); END;           00078900
         WRITE(PRINT[PAGE]); PAGENUMBER~RECORDNUMBER~0;                 00079000
     FILEID:=MFID:=0;                                                   00079100
        CLOSE(ANYTAPE[ALPHAORBINARYTAPE],RELEASE);ITSOVER:=TRUE;END;    00079200
    INTEGER STREAM PROCEDURE RECORDSIZE(R);                             00079300
    BEGIN SI~R; SI~SI-8; DI~ LOC RECORDSIZE;                            00079400
         DS~1 WDS; END;                                                 00079500
    PROCEDURE DOONELINE;                                                00079600
    BEGIN                                                               00079700
         IF LINECOUNT GEQ MAXLINES THEN TOPOFTAPEPAGE;                  00079800
         IF SINGLESPACE THEN WRITE(PRINT,15,PRINTIMAGE[*])              00079900
         ELSE WRITE(PRINT[DBL],15,PRINTIMAGE[*]);                       00080000
         IF SINGLESPACE THEN LINECOUNT~LINECOUNT +1                     00080100
                   ELSE LINECOUNT ~ LINECOUNT +2;                       00080200
          IF PUNCHIT THEN WRITE(PUNCH,10,PRINTLINE[*]);                 00080300
     IF COMMITSUICIDE THEN FINISHUP;                                    00080400
 END OF DOONELINE;                                                      00080500
BOOLEAN PROCEDURE ISITALINKEDRECORD;                                    00080600
BEGIN                                                                   00080700
INTEGER LINKWORD,PREVLINKWORD,LINKCHARVALUE;                            00080800
INTEGER VALU;                                                           00080900
BOOLEAN STREAM PROCEDURE FOUNDAFLAGBIT(TESTWORD);                       00081000
BEGIN SI:=    TESTWORD;DI:=LOC FOUNDAFLAGBIT;DI:=DI+7;                  00081100
     IF SB THEN DS:=LIT"1"ELSE DS:=LIT"0";                              00081200
END OF CHECKFORFLAGBIT;                                                 00081300
LABEL FORWARDLINKSOK,NOTLINKEDRECORDS,BACKWARDLINKSOK,EXITLINKCHECK;    00081400
DEFINE RECORDIMAGE=DISKIMAGE#,                                          00081500
       FORWARDLINKVALUE=                                                00081600
         IF     FOUNDAFLAGBIT(RECORDIMAGE[LINKWORD])                    00081700
         THEN 88888                                                     00081800
         ELSE IF(LINKCHARVALUE:=RECORDIMAGE[LINKWORD].[24:6]|1000       00081900
                               +RECORDIMAGE[LINKWORD].[30:6]|100        00082000
                               +RECORDIMAGE[LINKWORD].[36:6]|10         00082100
                               +RECORDIMAGE[LINKWORD].[42:6])           00082200
                 MOD 8 =0 THEN LINKCHARVALUE DIV 8                      00082300
                 ELSE 99999#,                                           00082400
       BACKWARDLINKVALUE=                                               00082500
         IF     FOUNDAFLAGBIT(RECORDIMAGE[LINKWORD])                    00082600
         THEN  88888                                                    00082700
         ELSE IF(LINKCHARVALUE:=RECORDIMAGE[LINKWORD].[1:5]|1000        00082800
                               +RECORDIMAGE[LINKWORD].[6:6]|100         00082900
                               +RECORDIMAGE[LINKWORD].[12:6]|10         00083000
                               +RECORDIMAGE[LINKWORD].[18:6])           00083100
                 MOD 8=0 THEN LINKCHARVALUE DIV 8                       00083200
                 ELSE 99999    #                                        00083300
;                                                                       00083400
     NUMBEROFLINKWORDS:=2;                                              00083500
     NOOFTAPELINES:=3;                                                  00083600
     BLOCKISCARDIMAGES:=TRUE;                                           00083700
     PREVLINKWORD:=LINKWORD:=0;                                         00083800
     WHILE LINKWORD:=PREVLINKWORD+VALU :=FORWARDLINKVALUE LSS 1024      00083900
     AND LINKWORD LSS THERECORDSIZE                                     00084000
     DO   BEGIN                                                         00084100
          IF VALU:=FORWARDLINKVALUE=0                                   00084200
          THEN IF LINKWORD=THERECORDSIZE-1                              00084300
               THEN GO FORWARDLINKSOK ELSE GO NOTLINKEDRECORDS;         00084400
          IF BLOCKISCARDIMAGES                                          00084500
          THEN IF LINKWORD-PREVLINKWORD NEQ 11                          00084600
               THEN BLOCKISCARDIMAGES:=FALSE;                           00084700
          NUMBEROFLINKWORDS:=NUMBEROFLINKWORDS+1;                       00084800
          NOOFTAPELINES:=NOOFTAPELINES+1                                00084900
                        +((LINKWORD-PREVLINKWORD-1)DIV 10)              00085000
                        +(IF(LINKWORD-PREVLINKWORD-1)MOD 10=0           00085100
                          THEN 0 ELSE 1);                               00085200
          PREVLINKWORD:=LINKWORD;                                       00085300
          END OF CHECKFFORWARDLLINKS;                                   00085400
     GO NOTLINKEDRECORDS;                                               00085500
FORWARDLINKSOK:                                                         00085600
     PREVLINKWORD:=LINKWORD;                                            00085700
     WHILE LINKWORD:=PREVLINKWORD-VALU :=BACKWARDLINKVALUE GTR-1        00085800
     DO   BEGIN                                                         00085900
          IF VALU :=BACKWARDLINKVALUE=0                                 00086000
          THEN IF LINKWORD=0                                            00086100
               THEN GO BACKWARDLINKSOK ELSE GO NOTLINKEDRECORDS;        00086200
          PREVLINKWORD:=LINKWORD;                                       00086300
          END;                                                          00086400
NOTLINKEDRECORDS:                                                       00086500
     NUMBEROFLINKWORDS:=NOOFTAPELINES:=0;                               00086600
     BLOCKISCARDIMAGES:=FALSE;                                          00086700
     ISITALINKEDRECORD:=FALSE;                                          00086800
     GO EXITLINKCHECK;                                                  00086900
BACKWARDLINKSOK:                                                        00087000
     ISITALINKEDRECORD:=TRUE;                                           00087100
EXITLINKCHECK: END OF ISITALINKEDRECORD;                                00087200
    PROCEDURE DATAPRNT;                                                 00087300
    BEGIN                                                               00087400
INTEGER POSDIV64X64,XTRAPOSDIV64,POSMOD64,ALINE,PRINTLINECHARS,POSITION,00087500
        NEXTLINKLOCNCHARS,POSITIONEND,CHARSDIV40,CHARSMOD40;            00087600
INTEGER VALU;                                                           00087700
BOOLEAN ITLOOKSLIKELINKEDRECORDS;                                       00087800
FORMAT NARROWBLOCKHDG("FILE "I3", BLOCK "I4", TYPE "A5                  00087900
                      ", BLOCK LENGTH "I5", MODE "A6);                  00088000
STREAM PROCEDURE TRANSFER(DISKIMAGE,PRINTLINE,POSDIV64X64,              00088100
     XTRAPOSDIV64,POSMOD64,CHARSDIV40,CHARSMOD40);                      00088200
VALUE POSDIV64X64,XTRAPOSDIV64,POSMOD64,CHARSDIV40,CHARSMOD40;          00088300
    BEGIN DI~PRINTLINE; DS~8LIT" "; SI~PRINTLINE; DS~9WDS;              00088400
    SI~DISKIMAGE;                                                       00088500
     POSDIV64X64(63(SI:=SI+63;SI:=SI+1);SI:=SI+63;SI:=SI+1);            00088600
     XTRAPOSDIV64(SI:=SI+63;SI:=SI+1);SI:=SI+POSMOD64;DI:=PRINTLINE;    00088700
         SI~SI-1;                                                       00088800
     CHARSDIV40(DS:=40CHR);DS:=CHARSMOD40 CHR;END;                      00088900
STREAM PROCEDURE FILLONE(PRINTIMAGE,FILENO,BLOCKNO,POSITION,PRINTLINE,  00089000
                         PARITY,OUTSWITCH,LABSWITCH,POS2,WIDEPAPER,     00089100
                         CHARSDIV40,CHARSMOD40);                        00089200
VALUE FILENO,BLOCKNO,POSITION,PARITY,OUTSWITCH,LABSWITCH,POS2,WIDEPAPER,00089300
      CHARSDIV40,CHARSMOD40;                                            00089400
    BEGIN                                                               00089500
    LABEL BYLABEL,NEXTFLD;                                              00089600
         DI~PRINTIMAGE; DS~8LIT" "; SI~PRINTIMAGE;                      00089700
         DS~14 WDS;                                                     00089800
     WIDEPAPER(                                                         00089900
         SI~LOC OUTSWITCH; SI~SI+1;                                     00090000
         IF SC = "C" THEN GO TO NEXTFLD;                                00090100
         SI~LOC PARITY; SI~SI+1; DI~PRINTIMAGE;                         00090200
         2(DI~DI+56); DS~7 CHR;                                         00090300
         SI~LOC LABSWITCH; SI~SI+1; IF SC ="L" THEN GO TO BYLABEL;      00090400
         SI~LOC FILENO; DI~PRINTIMAGE;                                  00090500
         DS~3DEC; DI~DI-3; DS~3FILL;                                    00090600
         DI~PRINTIMAGE; DI~DI+5;                                        00090700
         SI~LOC BLOCKNO; DS~4DEC; DI~DI-4; DS~4FILL;                    00090800
         DI~PRINTIMAGE; DI~DI+11;                                       00090900
         SI~LOC LABSWITCH; SI~SI+1; IF SC ="L" THEN GO TO BYLABEL;      00091000
         DS~6LIT"DATA  "; GO TO NEXTFLD;                                00091100
    BYLABEL: DI~PRINTIMAGE; DI~DI+11; DS~6LIT"LABEL ";                  00091200
    NEXTFLD: DI~PRINTIMAGE; DI~DI+16;                                   00091300
         SI~LOC POSITION;                                               00091400
         DS~5DEC; DI~DI-5; DS~5FILL;                                    00091500
         SI~LOC POS2; DI~PRINTIMAGE; DI~DI+21;                          00091600
         DS~5DEC; DI~DI-5; DS~5FILL;                                    00091700
     );                                                                 00091800
          DI:=PRINTIMAGE;WIDEPAPER(DI:=DI+28);DS:=LIT"<";               00091900
          SI:=PRINTLINE;CHARSDIV40(DS:=40CHR);DS:=CHARSMOD40 CHR;       00092000
         DS~1LIT">";                                                    00092100
         END;                                                           00092200
     IF ITLOOKSLIKELINKEDRECORDS:=ISITALINKEDRECORD                     00092300
     THEN BEGIN                                                         00092400
          NEXTLINKLOCNCHARS:=1;                                         00092500
          COMMENT NOOFTAPELINES HAS BEEN SET UP IN ISITALINKEDRECORD;   00092600
          END                                                           00092700
     ELSE BEGIN                                                         00092800
          NOOFTAPELINES:=THERECORDSIZE DIV 10                           00092900
                         +(IF THERECORDSIZE MOD 10=0 THEN 0 ELSE 1);    00093000
          IF THERECORDSIZE MOD 10 =0 AND LABSWITCH NEQ "LABEL  "        00093100
          THEN BLOCKISCARDIMAGES:=TRUE                                  00093200
          ELSE BLOCKISCARDIMAGES:=FALSE;                                00093300
          END;                                                          00093400
     PRINTLINECHARS:=POSITION:=0;                                       00093500
     FOR ALINE:=1 STEP 1 UNTIL NOOFTAPELINES                            00093600
     DO   BEGIN                                                         00093700
          OUTSWITCH:=IF ALINE GTR 1 THEN "CONTINU"ELSE"       ";        00093800
          POSITION:=IF ALINE=1 THEN 1 ELSE                              00093900
                    POSITION+PRINTLINECHARS;%AT THIS POINT PRINTLINECHAR00094000
%                         CONTAINS THE PREV PRINTED LINE LENGTH         00094100
          POSDIV64X64:=POSITION DIV 64 DIV 64;                          00094200
          XTRAPOSDIV64:=POSITION DIV 64 MOD 64;                         00094300
          POSMOD64:=POSITION MOD 64;                                    00094400
          PRINTLINECHARS:=IF NOT ITLOOKSLIKELINKEDRECORDS               00094500
                          THEN IF ALINE =NOOFTAPELINES                  00094600
                               THEN THERECORDSIZE|8-POSITION+1 ELSE 80  00094700
                          ELSE                                          00094800
                          IF NEXTLINKLOCNCHARS=POSITION THEN 8 ELSE     00094900
                          IF POSITION+80 LSS NEXTLINKLOCNCHARS          00095000
                          THEN 80 ELSE NEXTLINKLOCNCHARS-POSITION;      00095100
          CHARSDIV40:=PRINTLINECHARS DIV 40;                            00095200
          CHARSMOD40:=PRINTLINECHARS MOD 40;                            00095300
          POSITIONEND:=POSITION+PRINTLINECHARS-1;                       00095400
          IF(NOT NARROWPAPER)                                           00095500
          OR(NARROWPAPER AND NOT BLOCKISCARDIMAGES)                     00095600
          OR(NARROWPAPER AND BLOCKISCARDIMAGES                          00095700
               AND NEXTLINKLOCNCHARS NEQ POSITION)                      00095800
                              %THIS PREVENTS PRINTING OF LINKWORDS      00095900
                              %IF CARDIMAGES AND NARROWPPER             00096000
          THEN BEGIN                                                    00096100
          TRANSFER(DISKIMAGE,PRINTLINE,POSDIV64X64,XTRAPOSDIV64,POSMOD6400096200
                   ,CHARSDIV40,CHARSMOD40);                             00096300
          IF LINECOUNT GEQ MAXLINES THEN OUTSWITCH :="       ";         00096400
          FILLONE(PRINTIMAGE,FILENO,BLOCKNO,POSITION,PRINTLINE,PARITY,  00096500
                  OUTSWITCH,LABSWITCH,POSITIONEND,                      00096600
                  IF NARROWPAPER THEN 0 ELSE 1,                         00096700
                  CHARSDIV40,CHARSMOD40);                               00096800
          IF NARROWPAPER AND NOT BLOCKISCARDIMAGES                      00096900
          AND OUTSWITCH NEQ "CONTINU" AND LABSWITCH NEQ"LABEL  "        00097000
          THEN BEGIN                                                    00097100
               IF LINECOUNT+1 GEQ MAXLINES THEN TOPOFTAPEPAGE;          00097200
               WRITE(PRINT,NARROWBLOCKHDG,                              00097300
                     FILENO,BLOCKNO,                                    00097400
                     IF LABSWITCH="LABEL  "THEN"LABEL"ELSE" DATA",      00097500
                     THERECORDSIZE|8,PARITY.[6:36]);                    00097600
               LINECOUNT:=LINECOUNT+1;                                  00097700
               END;                                                     00097800
          DOONELINE;                                                    00097900
          END;                                                          00098000
     IF NEXTLINKLOCNCHARS=POSITION                                      00098100
     THEN NEXTLINKLOCNCHARS:=NEXTLINKLOCNCHARS                          00098200
          +VALU:=DISKIMAGE[NEXTLINKLOCNCHARS DIV 8].[24:6]|1000         00098300
                +DISKIMAGE[NEXTLINKLOCNCHARS DIV 8].[30:6]|100          00098400
                +DISKIMAGE[NEXTLINKLOCNCHARS DIV 8].[36:6]|10           00098500
                +DISKIMAGE[NEXTLINKLOCNCHARS DIV 8].[42:6];             00098600
          IF ITSOVER THEN ALINE:=NOOFTAPELINES;                         00098700
          END;                                                          00098800
     IF (NOT NARROWPAPER)OR(NARROWPAPER AND NOT BLOCKISCARDIMAGES)      00098900
     THEN % DONT SPACE BETWEEN BLOCKS IF NARROWPAPER CARDIMAGES         00099000
         IF LINECOUNT +1 LSS MAXLINES THEN                              00099100
               BEGIN WRITE(PRINT);LINECOUNT:=LINECOUNT+1;END            00099200
         ELSE TOPOFTAPEPAGE;                                            00099300
    END;                                                                00099400
    PROCEDURE LABELPRNT;                                                00099500
    BEGIN                                                               00099600
    STREAM PROCEDURE PULLOFFID(DISKIMAGE,FILEID,MFID);                  00099700
    BEGIN SI~DISKIMAGE; SI~SI+9; DI~MFID; DI~DI+1; DS~7 CHR;            00099800
         SI~SI+1; DI~FILEID; DI~DI+1; DS~7 CHR; END;                    00099900
         PULLOFFID(DISKIMAGE,FILEID,MFID);                              00100000
         LABSWITCH~"LABEL  ";                                           00100100
         FIRSTREAD~ FALSE;                                              00100200
         DATAPRNT; END;                                                 00100300
    PROCEDURE TAPEMKPRNT;                                               00100400
    BEGIN                                                               00100500
     BOOLEAN REMEMBERPUNCHIT;                                           00100600
STREAM PROCEDURE TPMARK(PRINTIMAGE,WIDEPAPER);VALUE WIDEPAPER;          00100700
    BEGIN DI~PRINTIMAGE; DS~8LIT" "; SI~PRINTIMAGE;                     00100800
          DS:=14WDS;DI:=PRINTIMAGE;WIDEPAPER(DI:=DI+10);                00100900
          DS:=9LIT"TAPE MARK";WIDEPAPER(DI:=DI+10);                     00101000
          DS:=63LIT"*";DS:=9LIT"*";WIDEPAPER(DS:=8LIT"*");              00101100
     END OF TPMARK;                                                     00101200
     TPMARK(PRINTIMAGE,IF NARROWPAPER THEN 0 ELSE 1);                   00101300
     IF PUNCHIT THEN BEGIN PUNCHIT:=FALSE;REMEMBERPUNCHIT:=TRUE END;    00101400
    DOONELINE;                                                          00101500
     IF REMEMBERPUNCHIT THEN BEGIN REMEMBERPUNCHIT:=FALSE;PUNCHIT:=TRUE 00101600
              END;                                                      00101700
         IF LINECOUNT +1 LSS MAXLINES THEN BEGIN                        00101800
          WRITE(PRINT);LINECOUNT:=LINECOUNT+1;END                       00101900
         ELSE TOPOFTAPEPAGE;                                            00102000
    END;                                                                00102100
    PROCEDURE READTHETAPE;                                              00102200
    BEGIN                                                               00102300
    STREAM PROCEDURE CHECKLABEL(DISKIMAGE,TLABEL);                      00102400
    BEGIN DI~TLABEL; DS~8LIT"0"; SI~DISKIMAGE; SI~SI+1;                 00102500
         DI~TLABEL; DI~DI+3; DS~ 5 CHR; END;                            00102600
         FORMAT BUFOVF("RECORD GTR THEN 1023 WDS- FILE ABT");           00102700
    FORMAT FMTY(2("********************"/),"**  PARITY ERROR  **"/,     00102800
  2("********************"/));                                          00102900
         LABEL PARCONT;                                                 00103000
    LABEL PARBRANCH,EOFBRANCH,CONT,GETOUT;                              00103100
    READ(ANYTAPE[ALPHAORBINARYTAPE][NO],1,DISKIMAGE[*])                 00103200
         [EOFBRANCH:PARBRANCH];                                         00103300
         IF ALPHAORBINARYTAPE = 1 THEN PARITY ~ "BINARY "               00103400
              ELSE PARITY ~ "ALPHA  ";                                  00103500
PARCONT:THERECORDSIZE:=RECORDSIZE(ANYTAPE[ALPHAORBINARYTAPE](0));       00103600
     IF THERECORDSIZE LEQ 1023 THEN                                     00103700
              BEGIN                                                     00103800
    READ(ANYTAPE[ALPHAORBINARYTAPE],THERECORDSIZE,DISKIMAGE[*])         00103900
                        [EOFBRANCH:PARBRANCH];                          00104000
                   GO TO CONT;                                          00104100
     END                                                                00104200
     ELSE BEGIN                                                         00104300
          WRITE(SPO,BUFOVF);FINISHUP;GO GETOUT;                         00104400
          END;                                                          00104500
    CONT:                                                               00104600
         CHECKLABEL(DISKIMAGE,TLABEL);                                  00104700
          IF TLABEL="LABEL" AND NOT INBLOCK THEN                        00104800
    BEGIN IF LABELEXPECT THEN LINECOUNT~MAXLINES; LABELPRNT;            00104900
         LABELREAD~TRUE;                                                00105000
         IF LABELEXPECT THEN BEGIN                                      00105100
              PREREADGAVETPMK~FALSE; LABELEXPECT~FALSE; END;            00105200
         IF PREREADGAVETPMK THEN LABELEXPECT ~TRUE; END                 00105300
    ELSE                                                                00105400
         BEGIN BLOCKNO~BLOCKNO+1;                                       00105500
              IF FIRSTREAD THEN FILENO~FILENO+1;                        00105600
              IF PREREADGAVETPMK THEN BEGIN FILENO~FILENO+1;            00105700
              BLOCKNO~1; END;                                           00105800
              IF LABELEXPECT THEN BEGIN                                 00105900
         FINISHUP; GO TO GETOUT; END;                                   00106000
              LABELREAD~ FALSE;                                         00106100
         PREREADGAVETPMK~FALSE;                                         00106200
         LABSWITCH~"       ";                                           00106300
         LABELEXPECT~FALSE;                                             00106400
    FIRSTREAD~FALSE;                                                    00106500
          INBLOCK:=TRUE;                                                00106600
         DATAPRNT; END;                                                 00106700
         GO TO GETOUT;                                                  00106800
    PARBRANCH: PARITY~ "PARITY ";                                       00106900
         IF LINECOUNT +5 GTR MAXLINES THEN TOPOFTAPEPAGE;               00107000
         WRITE(PRINT,FMTY); LINECOUNT~LINECOUNT+6;                      00107100
    GO TO PARCONT;                                                      00107200
     EOFBRANCH:                                                         00107300
          IF CARDECK OR SPECIFICTAPE THEN BEGIN FINISHUP;GO GETOUT END; 00107400
          PREREADGAVETPMK:=TRUE;TAPEMKPRNT;                             00107500
          INBLOCK:= FALSE;                                              00107600
         IF LABELEXPECT THEN FINISHUP;                                  00107700
    GETOUT: END;                                                        00107800
    PROCEDURE INNERLOOP;                                                00107900
    BEGIN                                                               00108000
    LABEL EXITFROMHERE,RPT;                                             00108100
    IF SUFFIX = "BINARY " OR SUFFIX = "ALPHA  " THEN                    00108200
     WRITE(SPO,TAPEFILE);                                               00108300
    LABELEXPECT~FALSE; LABELREAD ~ FALSE;                               00108400
          INBLOCK:=FALSE;                                               00108500
         FILENO~BLOCKNO~0;                                              00108600
    FIRSTREAD~TRUE; PREREADGAVETPMK~FALSE;                              00108700
    RPT:                                                                00108800
    READTHETAPE;                                                        00108900
         IF ITSOVER THEN GO TO EXITFROMHERE;                            00109000
    GO TO RPT;                                                          00109100
    EXITFROMHERE: END;                                                  00109200
    PROCEDURE TAPESECTIONOUTER;                                         00109300
    BEGIN                                                               00109400
         SETUPDATEANDTIME;                                              00109500
         LINECOUNT~MAXLINES; ITSOVER ~FALSE;                            00109600
         INNERLOOP;                                                     00109700
         END;                                                           00109800
    PROCEDURE FIRSTRECORDREAD;                                          00109900
         BEGIN                                                          00110000
         LABEL TPEMRKREAD,BINARYPARITYERR, ENDFIRSTRECORDREAD;          00110100
         LABEL BINARYREAD,NONBINARYREAD;                                00110200
         LABEL CHECKRECORDCOUNT,REWINDTAPE,NONBINARYPARITYERR;          00110300
         FORMAT READERROR("FIRST 10 RECORDS UNREADABLE");               00110400
         FORMAT TPEMRKERR("TAPE MARK AT RECORD NO. ",I2,                00110500
                          "PRECEEDS VALID DATA");                       00110600
         PARITYCOUNT:=RECORDCOUNT:= 0;                                  00110700
CHECKRECORDCOUNT:                                                       00110800
     WRITE(SPO,TAPEFILE);                                               00110900
         PARITYCOUNT:=RECORDCOUNT:=RECORDCOUNT + 1;                     00111000
         IF RECORDCOUNT GTR 11 THEN BEGIN                               00111100
              WRITE(SOURCE[WHICHSOURCE],READERROR);                     00111200
              GO TO STOPRUN;                                            00111300
              END;                                                      00111400
         IF RECORDCOUNT GTR 1 THEN                                      00111500
              CLOSE(ANYTAPE[ALPHAORBINARYTAPE],RELEASE);                00111600
         ALPHAORBINARYTAPE ~ 1;                                         00111700
         BINARYREAD:                                                    00111800
         READ(ANYTAPE[ALPHAORBINARYTAPE],1,DISKIMAGE[*])                00111900
              [TPEMRKREAD:BINARYPARITYERR];                             00112000
REWINDTAPE:                                                             00112100
    REWIND(ANYTAPE[ALPHAORBINARYTAPE]);                                 00112200
         GO TO ENDFIRSTRECORDREAD;                                      00112300
    BINARYPARITYERR:                                                    00112400
         IF PARITYCOUNT GTR 1 THEN                                      00112500
              BEGIN                                                     00112600
              PARITYCOUNT:=PARITYCOUNT - 1;                             00112700
              GO TO BINARYREAD; END;                                    00112800
         CLOSE(ANYTAPE[ALPHAORBINARYTAPE],RELEASE);                     00112900
         ALPHAORBINARYTAPE ~0;                                          00113000
     WRITE(SPO,TAPEFILE);                                               00113100
NONBINARYREAD:                                                          00113200
         READ(ANYTAPE[ALPHAORBINARYTAPE],1,DISKIMAGE[*])                00113300
              [:NONBINARYPARITYERR];                                    00113400
         GO TO REWINDTAPE;                                              00113500
    NONBINARYPARITYERR:                                                 00113600
         IF PARITYCOUNT GTR 1 THEN                                      00113700
              BEGIN                                                     00113800
              PARITYCOUNT:=PARITYCOUNT - 1;                             00113900
              GO TO NONBINARYREAD;                                      00114000
              END ELSE GO TO CHECKRECORDCOUNT;                          00114100
    TPEMRKREAD:                                                         00114200
         IF RECORDCOUNT < 3 THEN                                        00114300
              BEGIN                                                     00114400
              CLOSE(ANYTAPE[ALPHAORBINARYTAPE],*);                      00114500
              GO TO CHECKRECORDCOUNT;                                   00114600
              END;                                                      00114700
         WRITE(SOURCE[WHICHSOURCE],TPEMRKERR,RECORDCOUNT);              00114800
         GO TO STOPRUN;                                                 00114900
    ENDFIRSTRECORDREAD:END;                                             00115000
PROCEDURE CARDINPUT;                                                    00115100
BEGIN                                                                   00115200
     SETUPDATEANDTIME;                                                  00115300
     LABELEXPECT:=LABELREAD:=INBLOCK:=PREREADGAVETPMK:=FALSE;           00115400
     FIRSTREAD:=TRUE;                                                   00115500
     LINECOUNT:=MAXLINES;                                               00115600
     ITSOVER:=FALSE;                                                    00115700
     CARDECK:=TRUE;                                                     00115800
     ALPHAORBINARYTAPE:=1;                                              00115900
     IF NARROWPAPER THEN REMEMBERNARROWPAPER:=TRUE;                     00116000
     NARROWPAPER:=TRUE;                                                 00116100
     FILL BINARYTAPE WITH 0,SUFFIX;                                     00116200
     MFID:=0;FILEID:=SUFFIX;                                            00116300
     WHILE NOT ITSOVER  DO                                              00116400
     READTHETAPE;% TAPE PROCEDURE SHOULD NOW RUN OK ON CARD FILE        00116500
     CARDECK:=FALSE;                                                    00116600
     MFID:=FILEID:=0;                                                   00116700
     FILL BINARYTAPE WITH 0,"BINARYT";                                  00116800
     IF NOT REMEMBERNARROWPAPER THEN NARROWPAPER:=FALSE;                00116900
     REMEMBERNARROWPAPER:=FALSE;                                        00117000
 END OF CARDINPUT;                                                      00117100
PROCEDURE SPECIFICTAPEFILE;                                             00117200
BEGIN                                                                   00117300
     SPECIFICTAPE:=TRUE;                                                00117400
     LABELEXPECT:=LABELREAD:=INBLOCK:=PREREADGAVETPMK:=FALSE;           00117500
     FIRSTREAD:=TRUE;                                                   00117600
     FILENO:=BLOCKNO:=0;                                                00117700
     IF SUFFIX ="FILEA  "THEN ALPHAORBINARYTAPE:=0                      00117800
                         ELSE ALPHAORBINARYTAPE:=1;                     00117900
     WRITE(SPO,SPECTAPENAMEPLS);                                        00118000
     FETCHANDANALYZE;                                                   00118100
     FILL ANYTAPE[ALPHAORBINARYTAPE]WITH PREFIX,SUFFIX;                 00118200
     MFID:=PREFIX;FILEID:=SUFFIX;                                       00118300
     SETUPDATEANDTIME;LINECOUNT:=MAXLINES;ITSOVER:=FALSE;               00118400
     WHILE NOT ITSOVER DO READTHETAPE;                                  00118500
     MFID:=FILEID:=0;                                                   00118600
     IF ALPHAORBINARYTAPE=0 THEN FILL ALPHATAPE WITH 0,"ALPHATA"        00118700
                            ELSE FILL BINARYTAPE WITH 0,"BINARYT";      00118800
     FILENO:=BLOCKNO:=0;                                                00118900
     SPECIFICTAPE:= FALSE;                                              00119000
END OF SPECIFICTAPEFILE;                                                00119100
STARTTHEPROGRAM;                                                        00119200
GETANOTHER:                                                             00119300
    FETCHANDANALYZE;                                                    00119400
          IF PREFIX="CARDECK"THEN BEGIN CARDINPUT;GO GETANOTHER END;    00119500
         IF PREFIX = "TAPE   " THEN                                     00119600
              BEGIN                                                     00119700
              IF SUFFIX = "BINARY " THEN ALPHAORBINARYTAPE:= 1          00119800
              ELSE                                                      00119900
              IF SUFFIX = "ALPHA  " THEN ALPHAORBINARYTAPE:= 0          00120000
              ELSE                                                      00120100
              IF SUFFIX = "PRINT  " THEN FIRSTRECORDREAD                00120200
               ELSE IF SUFFIX="FILE   "OR SUFFIX="FILEA  "              00120300
                    THEN BEGIN SPECIFICTAPEFILE;GO GETANOTHER END       00120400
              ELSE GO TO NOTTAPEINPUT;                                  00120500
              TAPESECTIONOUTER;                                         00120600
              GO TO GETANOTHER;                                         00120700
    NOTTAPEINPUT:END;                                                   00120800
     IF PREFIX="=      " THEN ALLPREF:=SEARCHDIR:=TRUE;                 00120900
     IF SUFFIX="=      " THEN ALLSUFF:=SEARCHDIR:=TRUE;                 00121000
     IF NOT SEARCHDIR                                                   00121100
     THEN BEGIN                                                         00121200
          DOYOURTHING;                                                  00121300
          GO TO GETANOTHER;                                             00121400
          END;                                                          00121500
     DIRECKEY:=-1;                                                      00121600
     NULLFILES:=TRUE;                                                   00121700
     WHILE SEARCHDIR                                                    00121800
     DO   BEGIN                                                         00121900
          DIRECKEY:=DIRECKEY+16;                                        00122000
          READ(DIRECTORY[DIRECKEY],30,DIRECORD[*]);                     00122100
          FOR POINTER:=28 STEP-2 UNTIL 0                                00122200
          DO   BEGIN                                                    00122300
               IF DIRECORD[POINTER]=76%DIRECTORY END OF FILE            00122400
               THEN BEGIN                                               00122500
                    IF NULLFILES                                        00122600
                    THEN WRITE(SOURCE[WHICHSOURCE],NULLFILEGROUP,       00122700
                               IF ALLPREF THEN"      "ELSE PREFIX.[6:36]00122800
                               ,IF ALLPREF THEN"="ELSE PREFIX,          00122900
                               IF ALLSUFF THEN"=     "ELSE SUFFIX.[6:36]00123000
                               ,IF ALLSUFF THEN" "ELSE SUFFIX);         00123100
                    SEARCHDIR:=ALLPREF:=ALLSUFF:=FALSE;                 00123200
                    POINTER:=DIRECKEY:=-50;                             00123300
                    GO TO FREAKOUT                                      00123400
                    END;                                                00123500
               IF ALLPREF THEN PREFIX:=DIRECORD[POINTER];               00123600
               IF ALLSUFF THEN SUFFIX:=DIRECORD[POINTER+1];             00123700
               IF DIRECORD[POINTER] NEQ 12%12 MEANS NULL ENTRY          00123800
               THEN BEGIN                                               00123900
                    IF PREFIX=DIRECORD[POINTER]                         00124000
                    AND SUFFIX=DIRECORD[POINTER+1]                      00124100
                    THEN BEGIN                                          00124200
                         READ(DIRECTORY[DIRECKEY-16+((POINTER+2)DIV 2)],00124300
                              30,DIRFILEHDR[*]);                        00124400
                         IF DIRFILEHDR[4].[10:1] NEQ 1                  00124500
                         THEN                                           00124600
                         BEGIN                                          00124700
                         WRITE(SOURCE[WHICHSOURCE],                     00124800
                               NEXTFILE,FORMATFILENAME);                00124900
                         DOYOURTHING;                                   00125000
                         NULLFILES:=FALSE;                              00125100
                         END                                            00125200
                         ELSE WRITE(SOURCE[WHICHSOURCE],BYPASSOBJECTFILE00125300
                                   ,FORMATFILENAME);                    00125400
                         END; %OF PROCESSING A FILE NAME HIT            00125500
                    END;                                                00125600
FREAKOUT:                                                               00125700
               END;                                                     00125800
          END;                                                          00125900
     GO TO GETANOTHER;                                                  00126000
 STOPRUN:                                                               00126100
 END.                                                                   00126200
