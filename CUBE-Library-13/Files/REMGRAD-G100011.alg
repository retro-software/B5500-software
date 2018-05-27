          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS G100011.  THE PROGRAM NAME IS     00000200
               "REMOTE/GRADER".  THIS VERSION DATED 5/3/68;             00000300
COMMENT        MICHAEL MALCOLM, REMOTE UNIT GRADER;                     00000400
ALPHA FILE IN  TTIN 14(1,8);                                            00000500
ALPHA FILE OUT TTOUT 14(1,8);                                           00000600
FILE OUT       LINE 18(2,15);                                           00000700
FORMAT         FO1(X8,2A1,"PLEASE TYPE CLASS ID",3A1),                  00000800
               FO2(X8,3A1),                                             00000900
               FO3(X8,4A1,"MEAN=",F6.2,X3,"STANDARD DEVIATION=",F5.2,   00001000
                 3A1),                                                  00001100
               FO5(   2A6),                                             00001200
               FO6(X8,2A1,"TYPE IN STUDENTS NAMES ONE AT A TIME",3A1),  00001300
               FO8(X8,2A1,"HOW MANY STUDENTS ARE IN THE CLASS.",3A1),   00001400
               FO9(X8,2A1,"FIRST TYPE WEIGHTING FACTOR AND THEN THE",   00001500
               " GRADES",3A1),                                          00001600
               FO10(X8,2A1,"WEIGHTING FACTOR =",1A1),                   00001700
               FO11(X8,2A1,2A6," :",A1),                                00001800
               FO12(X8,3A1,"HERE IS THE CLASS LISTED FROM BEST TO WOR", 00001900
               "ST",3A1),                                               00002000
               FO13(X8,2A1,2A6," :",I4,2A1),                            00002100
               FO14(X8,3A1,"DO YOU HAVE ANY MORE GRADES TO ENTER AT TH",00002200
               "IS TIME.",3A1),                                         00002300
               FO17(X8,2A1,"THE SUM OF THE WEIGHTING FACTORS TO DATE IS"00002400
               ," :",F6.2,3A1),                                         00002500
               FO18(X8,2A1,"GRADE AVERAGES TO DATE ARE :",3A1),         00002600
               FO19(X8,3A1,"GRADE AVERAGES FROM BEST TO WORST ARE:",    00002700
                   3A1),                                                00002800
               FO20(X8,3A1,"DO YOU WANT A GRAPH MADE.",3A1),            00002900
               FO21(X8,2A1,"YOU MAY PICK IT UP AT THE COMPUTING CENTER."00003000
                 ,3A1),                                                 00003100
               GRAPH(X3,2A6," :",100A1),                                00003200
               IDLINE(X20,2A6,/),                                       00003300
               LABL(X17,"1",X7,"10",X8,"20",X8,"30",X8,"40",X8,"50",X8, 00003400
                "60",X8,"70",X8,"80",X8,"90",X7,"100",/);               00003500
ALPHA          CR,LF,LA,   YESNO,DCIM,ID1,ID2;                          00003600
ARRAY          RD[0:7];                                                 00003700
INTEGER        STA,I,J,N,GT,GMAX,IT;                                    00003800
ALPHA ARRAY    N1,N2,   C1,C2[0:200];                                   00003900
LABEL          AGAIN;                                                   00004000
DEFINE         FORI = FOR I:=1 STEP 1 UNTIL #,                          00004100
               FORJ = FOR J:=1 STEP 1 UNTIL #,                          00004200
               CRLFIT = CR,LF,CR,LF,LA#;                                00004300
REAL           WF,MEAN,SIGMA,SDIM,WFT;                                  00004400
        ARRAY  GRAD,GORD,GDUM[0:200];                                   00004500
REAL ARRAY     GRADTOT[0:200];                                          00004600
LIST           L1(ID1,ID2),                                             00004700
               L2(N1[I],N2[I]),                                         00004800
               L3(C1[I],C2[I],      FORJ GORD[I]-1 DO " ","+");         00004900
BOOLEAN STREAM PROCEDURE DECIMAL(B,NDG,DCIM);                           00005000
VALUE          NDG;                                                     00005100
          BEGIN                                                         00005200
COMMENT        TRUE IF THE NUMBER CONTAINS A DECIMAL POINT;             00005300
               SI:=B; DI:=DCIM;  DI:=DI+7;                              00005400
               NDG(IF SC=DC THEN TALLY := 1; DI:=DI-1);                 00005500
               DECIMAL:=TALLY;                                          00005600
          END OF DECIMAL;                                               00005700
STREAM PROCEDURE PUTITIN(B,DCIM,NDG);                                   00005800
VALUE          NDG;                                                     00005900
          BEGIN                                                         00006000
COMMENT        PUTS DECIMAL IN AFTER INTEGER PART;                      00006100
               DI:=B;  SI:=DCIM;  SI:=SI+7;  DI:=DI+NDG;                00006200
               DS:=1 CHR; DS:= 20 LIT "0";                              00006300
          END OF PUTITIN;                                               00006400
INTEGER STREAM PROCEDURE FINDGP(B,LA);                                  00006500
          BEGIN                                                         00006600
COMMENT        THE NUMBER OF CHARACTERS PRECEDING THE GROUP MARK IS     00006700
               FOUND;                                                   00006800
LABEL          FOUNDIT;                                                 00006900
               DI~LA;  DI~DI+7;                                         00007000
               SI:=B;   20(IF SC=DC THEN JUMP OUT 1 TO FOUNDIT;         00007100
               DI:=DI-1; TALLY:= TALLY +1);                             00007200
FOUNDIT:       FINDGP:=TALLY;                                           00007300
          END  OF FINDGP;                                               00007400
STREAM PROCEDURE MOVE(F,T,N);                                           00007500
VALUE          N;                                                       00007600
          BEGIN                                                         00007700
COMMENT        PREPARES T TO BE READ IN AN R20.10 FORMAT;               00007800
               SI:=F;DI:=T;DS:=N CHR;DS:=20 LIT "0";                    00007900
          END  OF MOVE;                                                 00008000
STREAM PROCEDURE MOVEBLANKS(F,T,N);                                     00008100
VALUE          N;                                                       00008200
          BEGIN                                                         00008300
               SI:=F; DI:=T; DS:=N CHR; DS:=20 LIT " ";                 00008400
          END OF MOVEBLANKS;                                            00008500
PROCEDURE      READVAL(V,STATION);                                      00008600
VALUE          STATION;                                                 00008700
INTEGER        STATION;                                                 00008800
REAL           V;                                                       00008900
          BEGIN                                                         00009000
COMMENT        THE VALUE OF V IS READ FROM THE REMOTE STATION THRU THE  00009100
               USE OF A MOVE AND EDIT;                                  00009200
ARRAY          RD1[0:7],RD2[0:4];                                       00009300
INTEGER        GP;                                                      00009400
FORMAT         F1(R20.10),                                              00009500
               FSO(X8,2A1,"TYPING ERROR.  START OVER",3A1);             00009600
LABEL          RER,DUNN,TRYAGAIN;                                       00009700
TRYAGAIN:      READ(TTIN(STATION),8,RD1[*]);                            00009800
               GP:= FINDGP(RD1[1],LA);                                  00009900
               IF NOT DECIMAL(RD1[1],GP,DCIM) THEN                      00010000
          BEGIN                                                         00010100
               PUTITIN(RD1[1],DCIM,GP);                                 00010200
          GP := GP + 1;                                                 00010300
          END;                                                          00010400
               MOVE(RD1[1],RD2[0],GP);                                  00010500
               READ(RD2[*],F1,V)[:RER];  GO TO DUNN;                    00010600
RER:           WRITE(TTOUT(STATION),FSO,CR,LF,LF,CR,LA);                00010700
               GO TO TRYAGAIN;                                          00010800
DUNN:     END  OF READVAL;                                              00010900
PROCEDURE      READNAME(LST,FMT);                                       00011000
LIST           LST;                                                     00011100
FORMAT         FMT;                                                     00011200
          BEGIN                                                         00011300
INTEGER        GP;                                                      00011400
ARRAY          RD1[0:7],RD2[0:4];                                       00011500
               READ(TTIN(STA),8,RD1[*]);                                00011600
               GP:= FINDGP(RD1[1],LA);                                  00011700
               MOVEBLANKS(RD1[1],RD2[0],GP);                            00011800
               READ(RD2[*],FMT,LST);                                    00011900
          END OF READNAME;                                              00012000
BOOLEAN PROCEDURE YES;                                                  00012100
          BEGIN                                                         00012200
FORMAT         FIN(X8,A2),                                              00012300
               FOUT(X8,2A1,"PLEASE TYPE YES OR NO."3A1);                00012400
ALPHA          YESNO;                                                   00012500
LABEL          GOOFED;                                                  00012600
GOOFED:        READ(TTIN(STA),FIN,YESNO);                               00012700
               IF YESNO = "YE" THEN YES := TRUE ELSE                    00012800
               IF YESNO = "NO" THEN YES := FALSE ELSE                   00012900
          BEGIN                                                         00013000
               WRITE(TTOUT(STA),FOUT,CRLFIT);                           00013100
               GO TO GOOFED                                             00013200
          END;                                                          00013300
          END OF YES;                                                   00013400
               CR:=47; LF:=60; LA:=31;    DCIM:=26;                     00013500
               STA~STATUS(RD[*]);                                       00013600
               STA.[9:9]~RD[0].[9:9];                                   00013700
               WRITE(TTOUT(STA),FO1,CRLFIT);                            00013800
               READNAME(L1,FO5);                                        00013900
               WRITE(TTOUT(STA),FO8,CRLFIT);                            00014000
               READVAL(N,STA);                                          00014100
               WRITE(TTOUT(STA),FO6,CR,LF,         LF,CR,LA);           00014200
               FORI N DO                                                00014300
          BEGIN                                                         00014400
               READNAME(L2,FO5);                                        00014500
               WRITE(TTOUT(STA),FO2,LF,CR,LA);                          00014600
          END  OF READING NAMES;                                        00014700
AGAIN:         WRITE(TTOUT(STA),FO9,LF,CR,         LF,CR,LA);           00014800
               WRITE(TTOUT(STA),FO10,CR,LF,LA);                         00014900
               READVAL(WF,STA);                                         00015000
               FORI  N DO                                               00015100
          BEGIN                                                         00015200
               WRITE(TTOUT(STA),FO11,CR,LF,N1[I],N2[I],      LA);       00015300
               READVAL(GRAD[I],STA);                                    00015400
          END  OF READING GRADES;   GT:=0;                              00015500
               FORI N DO                                                00015600
          BEGIN                                                         00015700
               GT:= GT + GRAD[I];                                       00015800
               GDUM[I]:=GRAD[I];                                        00015900
          END;                                                          00016000
               MEAN:=GT/N; SDIM:=0;                                     00016100
               FORI N DO                                                00016200
               SDIM :=SDIM +(GRAD[I] -MEAN)*2 ;   SIGMA :=SQRT(SDIM/N); 00016300
               WRITE(TTOUT(STA),FO3,CR,LF,LF,LF,MEAN,SIGMA,LF,CR,LA);   00016400
               WRITE(TTOUT(STA),FO12,CR,LF,LF,CR,LF,LA);                00016500
               GMAX:=0;                                                 00016600
               FORI N DO GRAD[I] := GRAD[I] + @-10;                     00016700
               FORI  N DO                                               00016800
          BEGIN                                                         00016900
               FORJ N DO                                                00017000
          BEGIN                                                         00017100
               IF GRAD[J] GTR GMAX THEN                                 00017200
          BEGIN                                                         00017300
               GMAX :=GRAD[J];   IT :=J;                                00017400
          END                                                           00017500
          END;                                                          00017600
               GRAD[IT]:=0;GORD[I] :=GMAX;  GMAX:=0;                    00017700
               C1[I]:=N1[IT];C2[I]:=N2[IT];                             00017800
               WRITE(TTOUT(STA),FO13,LF,CR,   C1[I],C2[I],      GORD[I],00017900
                   CR,LA);                                              00018000
          END;                                                          00018100
               FORI N DO                                                00018200
               GRADTOT[I] :=GRADTOT[I] + WF |    GDUM[I];               00018300
               WFT:=WFT+WF;                                             00018400
               WRITE(TTOUT(STA),FO14,LF,   CRLFIT);                     00018500
               IF YES             THEN GO TO AGAIN ;                    00018600
               WRITE(TTOUT(STA),FO17,CR,LF,WFT,CR,LF,LA);               00018700
               WRITE(TTOUT(STA),FO18,CRLFIT);                           00018800
               FORI N DO GRAD[I]:=GRADTOT[I]/WFT; FORI N DO             00018900
               WRITE(TTOUT(STA),FO13,CR,LF,   N1[I],N2[I],              00019000
                    GRAD[I],       CR,   LA);                           00019100
               GT:=0;                                                   00019200
               FORI N DO GT := GT + GRAD[I];                            00019300
               MEAN:= GT/N;  SDIM:=0;                                   00019400
               FORI N DO                                                00019500
               SDIM:=SDIM +(GRAD[I] - MEAN)*2;  SIGMA:=SQRT(SDIM/N);    00019600
               WRITE(TTOUT(STA),FO3,CR,LF,LF,LF,MEAN,SIGMA,LF,CR,LA);   00019700
               WRITE(TTOUT(STA),FO19,LF,CRLFIT);                        00019800
               GMAX:=0;                                                 00019900
               FORI N DO GRAD[I] := GRAD[I] + @-10;                     00020000
               FORI  N DO                                               00020100
          BEGIN                                                         00020200
               FORJ N DO                                                00020300
          BEGIN                                                         00020400
               IF GRAD[J] GTR GMAX THEN                                 00020500
          BEGIN                                                         00020600
               GMAX :=GRAD[J];   IT :=J;                                00020700
          END                                                           00020800
          END;                                                          00020900
               GRAD[IT]:=0;GORD[I] :=GMAX;  GMAX:=0;                    00021000
               C1[I]:=N1[IT];C2[I]:=N2[IT];                             00021100
               WRITE(TTOUT(STA),FO13,LF,CR,   C1[I],C2[I],      GORD[I],00021200
                   CR,LA);                                              00021300
          END;                                                          00021400
               WRITE(TTOUT(STA),FO20,LF,CRLFIT);                        00021500
               IF YES            THEN                                   00021600
          BEGIN                                                         00021700
               WRITE(LINE,IDLINE,L1);                                   00021800
               WRITE(LINE,LABL);                                        00021900
               FORI N DO                                                00022000
               WRITE(LINE,GRAPH,L3);                                    00022100
               WRITE(TTOUT(STA),FO21,CRLFIT);                           00022200
          END ELSE                                                      00022300
          END.                                                          00022400
