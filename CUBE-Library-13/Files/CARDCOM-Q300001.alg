BEGIN                                                                   00000100
COMMENT        PROGRAM - CARDCOM,                                       00000200
               CUBE LIBRARY NUMBER IS Q300001.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
COMMENT                                                                 00000500
               CARD COMPARATOR-"CARDCOM"                                00000600
     THIS PROGRAM IS A SERVICE ROUTINE WHICH WILL COMPARE TWO CARD      00000700
DECKS.  THESE DECKS MAY NOT CONTAIN ILLEGAL CHARACTERS, I.E., THEY MUST 00000800
VALID B5000 FILES.  OPTIONAL FEATURES OF THE PROGRAM ARE:               00000900
          A) A LISTING OF THE DECKS,                                    00001000
          B) ANOTHER COPY OF THE DECK,                                  00001100
          C) BOTH A) AND B) ABOVE.                                      00001200
                         ----------                                     00001300
     TO USE THIS ROUTINE, PLACE THE NECESSARY CONTROL CARDS AROUND THE  00001400
PROGRAM DECK.  THE CARD READER FILE NAME FOR THE DATA FILE IS "CR".  THE00001500
DATA DECK CONSISTS OF                                                   00001600
          1) A "$ OPTION CARD".  THIS HAS A $ IN COLUMN 1, AND ONE OF TH00001700
             FOLLOWING WORDS ELSEWHERE ON THE CARD                      00001800
             A) LIST - PROVIDES A LISTING                               00001900
             B) DECK - PROVIDES A THIRD DECK (IF THE TWO AGREE)         00002000
             C) BOTH - BOTH OF THE ABOVE                                00002100
             D) NONE - A PLAIN COMPARISON                               00002200
          2) THE FIRST, MASTER DECK,                                    00002300
          3) A SENTINEL CARD OF ALL * S,                                00002400
          4) THE SECOND, DUPLICATE DECK,                                00002500
          5) A SENTINEL CARD OF ALL * S.                                00002600
     IF ANOTHER COPY OF THE DECK IS REQUESTED, VIA DECK OR BOTH ON THE  00002700
OPTION CARD, IT IS ONLY PROVIDED IF THE TWO INPUT DECKS AGREE.          00002800
     ERROR INDICATIONS APPEAR ON THE LINE PRINTER, GIVING THE TWO CARD  00002900
IMAGES AND ALSO THE POSITION OF THE CARD IN THE DECK.  THERE ARE ALSO ER00003000
MESSAGES WHEN AN INCORRECT DATA DECK ARRANGEMENT IS DETECTED.           00003100
     THE PROGRAM S BASIC STRATEGY IS TO COPY THE MASTER DECK ONTO       00003200
MAGNETIC TAPE, AND THEN COMPARE THIS COPY ON A CARD-TO-CARD BASIS WITH T00003300
DUPLICATE DECK.  THE PROGRAM IS I/O BOUND.                              00003400
A.P. BATSON                                                             00003500
JULY, 1964                                                              00003600
                                                                       ;00003700
FILE CR(2,10); FILE LP 4 (2,15); FILE CPH 0 (2,10);                     00003800
                    COMMENT      THE DATA DECK FOR THIS PROGRAM IS IN   00003900
                              THREE PARTS. THE FIRST CARD IS A $ OPTION 00004000
                              CARD, WITH $ IN COLUMN 1. THIS CARD       00004100
                              MUST HAVE PUNCHED ON IT ONE OF THE        00004200
                              WORDS "LIST", "DECK", "BOTH", OR "NONE".  00004300
                              NEXT COMES THE MASTER DECK, FOLLOWED BY   00004400
                              A CARD OF ALL *, FOLLOWED BY THE          00004500
                              DUPLICATE DECK, FOLLOWED BY A CARD        00004600
                              OF ALL *. THE TWO DECKS ARE COMPARED,     00004700
                              AND A LISTING IS GIVEN, OR ANOTHER        00004800
                              DECK PUNCHED, OR BOTH, AS REQUESTED       00004900
                              ON THE FIRST CARD. NO DUPLICATE DECK      00005000
                              IS GIVEN IF THE TWO DECKS DISAGREE.       00005100
                              THE PRINTER LISTING ALSO GIVES THE        00005200
                              POSITION OF EACH CARD IN THE DECK.        00005300
                              THE FILEID IS CR, OF COURSE;              00005400
     FILE MM1 2(2,10);                                                  00005500
     BOOLEAN LISTING,DUPDECK;                                           00005600
     INTEGER I,I2;                                                      00005700
     REAL D;                                                            00005800
     ARRAY CARD1,CARD2[1:15];                                           00005900
     LABEL TERMINATE;                                                   00006000
     BEGIN                              COMMENT THIS IS THE INNER BLOCK 00006100
                                        WHERE ALL THE WORK GETS DONE;   00006200
          LABEL L1,GOOF1,GOOF2,L2,L3,BOTTOM,GOOF4,GOOF5,  L4;           00006300
          FORMAT SEQ(X100, I8, X12);                                    00006400
          FORMAT NOBOOL(X10,"INCORRECT $ OPTION CARD"),                 00006500
               F1(X10,"A LISTING WILL BE PROVIDED"),                    00006600
               F2(X10,"A THIRD COPY OF THE DECK WILL BE PROVIDED,"      00006700
                    ," BUT ONLY IF THE TWO DECKS AGREE"),               00006800
               F3(X10,"NO LISTING OR COPY OF DECK. NO FURTHER ",        00006900
                   "OUTPUT MEANS GOOD NEWS");                           00007000
          ARRAY ASTERISKS[1:15];                                        00007100
          FORMAT GOF1(X10,"THERE ARE NO ASTERISK CARDS IN YOUR DECK"),  00007200
               GOF2(X10,"YOUR SECOND DECK HAS LESS CARDS"),             00007300
               GOF4(X10,"IMPOSSIBLE. GO BUY ANOTHER COMPUTER");         00007400
          PROCEDURE CHECKIT(FILENAME,L);ARRAY FILENAME [1]; LABEL L;    00007500
               BEGIN BOOLEAN STREAM PROCEDURE C(CARD1, FILENAME);       00007600
                    BEGIN LABEL NOGO,GETOUT;                            00007700
                    SI~CARD1;DI~FILENAME;                               00007800
                    10(IF 8 SC=DC THEN SI~SI+0 ELSE                     00007900
                         JUMP OUT 1 TO NOGO );                          00008000
                    TALLY~ 1; GO TO GETOUT;                             00008100
                    NOGO:TALLY~ 0;                                      00008200
                    GETOUT:C~TALLY;                                     00008300
                    END;                                                00008400
               IF C(CARD1,FILENAME) THEN GO TO L;                       00008500
               END;                                                     00008600
                                                                        00008700
     ALPHA STREAM PROCEDURE DOLLARCARD(CARDIMAGE);                      00008800
     BEGIN   LABEL BADDOLLAR, AOK, GETIT, BOTTOM;                       00008900
          SI~CARDIMAGE; IF SC="$" THEN GO TO AOK ELSE GO TO BADDOLLAR;  00009000
          AOK: 60(SI~SI+1; IF SC=ALPHA THEN  JUMP OUT 1 TO GETIT);      00009100
               15(SI~SI+1; IF SC=ALPHA THEN  JUMP OUT 1 TO GETIT);      00009200
          BADDOLLAR: DI~LOC DOLLARCARD; DI~DI+4; DS~ 4 LIT "GLOP";      00009300
                    GO TO BOTTOM;                                       00009400
          GETIT: DI~LOC DOLLARCARD; DI~DI+4; DS~ 4 CHR;                 00009500
          BOTTOM:                                                       00009600
     END;                                                               00009700
          STREAM PROCEDURE BLANKARRAY(NAME);                            00009800
               BEGIN DI~NAME;                                           00009900
                    15(DS~ 8 LIT"        ");                            00010000
               END;                                                     00010100
                                                                        00010200
          STREAM PROCEDURE ASTERISKFILL(A);                             00010300
               BEGIN DI~ A;                                             00010400
                    15(DS~ 8 LIT"********");                            00010500
               END;                                                     00010600
          BLANKARRAY(CARD1);BLANKARRAY(CARD2);ASTERISKFILL(ASTERISKS);  00010700
          LISTING~DUPDECK~FALSE;                                        00010800
          I~0;     COMMENT NOW GET THE $ OPTION CARD;                   00010900
          READ(CR, 10,CARD1[*]);                                        00011000
          D~ DOLLARCARD(CARD1);                                         00011100
          IF D="LIST" THEN BEGIN   LISTING~TRUE;WRITE(LP,F1);           00011200
                                   GO TO L1 END;                        00011300
          IF D="DECK" THEN BEGIN   DUPDECK~TRUE; WRITE(LP,F2);          00011400
                                   GO TO L1 END;                        00011500
          IF D="BOTH" THEN BEGIN   LISTING~DUPDECK~TRUE;                00011600
                                   WRITE(LP,F1);WRITE(LP,F2);           00011700
                                   GO TO L1 END;                        00011800
          IF D!"NONE" THEN BEGIN WRITE(LP,NOBOOL);GO TO TERMINATE END;  00011900
          WRITE(LP,F3);                                                 00012000
          L1: READ(CR,10,CARD1[*])[GOOF1];                              00012100
                                        COMMENT READ DECK ONE;          00012200
          CHECKIT(ASTERISKS,L2);        COMMENT TO L2 IF ASTERISKS;     00012300
          I~I+1;                                                        00012400
          WRITE(MM1,10,CARD1[*]); GO TO L1;                             00012500
          L2: REWIND(MM1);              COMMENT NOW DECKONE IS ON TAPE; 00012600
          I2~0;    WRITE(LP[PAGE]);                                     00012700
          L3: IF I2= I THEN GO TO BOTTOM;                               00012800
          READ (CR,10,CARD1[*])[GOOF2];                                 00012900
          CHECKIT(ASTERISKS,GOOF2);                                     00013000
          READ (MM1,10,CARD2[*])[GOOF4];                                00013100
          CHECKIT(CARD2,L4); GO TO GOOF5;                               00013200
          L4: IF LISTING THEN BEGIN     WRITE(LP[NO],15,CARD2[*]);      00013300
                                        WRITE(LP,SEQ,I2+1);             00013400
                              END;                                      00013500
          I2~ I2+1; GO TO L3;                                           00013600
          GOOF1:WRITE(LP,GOF1); GO TO TERMINATE;                        00013700
          GOOF2:WRITE(LP,GOF2); GO TO TERMINATE;                        00013800
          GOOF4:WRITE(LP,GOF4); GO TO TERMINATE;                        00013900
          GOOF5:WRITE(LP,15,ASTERISKS[*]);                              00014000
                WRITE(LP[NO],15,CARD2[*]);                              00014100
                WRITE(LP,SEQ,I2+1);                                     00014200
                WRITE(LP[NO],15,CARD1[*]);                              00014300
                WRITE(LP[DBL],15,ASTERISKS[*]);                         00014400
                WRITE(LP,SEQ,I2+1);                                     00014500
                DUPDECK~FALSE; I2~I2+1; GO TO L3;                       00014600
          BOTTOM: REWIND(MM1); CLOSE(CR,RELEASE);                       00014700
          END *;                                                        00014800
     IF DUPDECK THEN                                                    00014900
          FOR D~ 1 STEP 1 UNTIL I DO BEGIN                              00015000
          READ(MM1,10,CARD1[*]);                                        00015100
          WRITE(CPH,10,CARD1[*]);       END;                            00015200
     TERMINATE:REWIND(MM1);                                             00015300
END.                                                                    00015400
