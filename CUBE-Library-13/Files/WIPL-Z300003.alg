REAL ESPDISK; % FOR TSS MCP IN CASE CALLED AS A VERB                %T1110138000
DEFINE MAX = 0 #; % SINGLE-USER                                     %T1110139000
DEFINE STATUS(STATUS1,STATUS2) = STATUS1#; %                        %T1110140000
$ VOIDT 10147001                                                    %T1110141000
REAL DUSERID; DEFINE USERID[USERID1] = DUSERID #; COMMENT %         %T1110164000
ARRAY DINFO[0:INFMAX]; DEFINE INFO[INFO1,INFO2] = DINFO[INFO2] #;   %T1110170000
COMMENT %                                                           %T1110170500
ARRAY DIDENTIFIERS[0:IDENTIFIERSARRAYSIZE]; %                       %T1110290000
DEFINE IDENTIFIERS[IDENTIFIERS1,IDENTIFIERS2] = %                   %T1110290400
      DIDENTIFIERS[IDENTIFIERS2] #; COMMENT %                       %T1110290600
REAL DRUNNER; DEFINE RUNNER[RUNNER1] = DRUNNER #; COMMENT %         %T1110324000
FILE IN ERRF DISK RANDOM "WIPLS" "ERRORS"(1,10,30); %               %T1110507100
PROCEDURE STARTUPMSG(TF); VALUE TF; BOOLEAN TF; FORWARD; %          %T1110513100
BOOLEAN BRAAAAAK;  SAVE ARRAY XXXXX[0:9]; %                         %T1120079005
PROCEDURE TWXOUTX(A,N,T); VALUE N,T; REAL A,N,T;                    %T1120079010
BEGIN COMMUNICATE(-11); BRAAAAAK:=BOOLEAN(T); END;                  %T1120079015
BOOLEAN PROCEDURE TWXOUT(A,N,T); VALUE N,T; REAL A,N,T;             %T1120079020
BEGIN INTEGER I,B,E; DEFINE X = XXXXX#;                             %T1120079025
STREAM PROCEDURE SC(A,X,B,E);                                       %T1120079026
BEGIN LOCAL G;                                                      %T1120079027
DI:=X; DS:=8 LIT " "; SI:=X; DS:=9 WDS;                             %T1120079028
DI:=X; SI:=A;                                                       %T1120079029
Y: 8(IF SC="!" THEN BEGIN SI:=SI+1; TALLY:=TALLY+1; END ELSE        %T1120079030
     IF SC="{" THEN BEGIN SI:=SI+1; TALLY:=TALLY+16; END ELSE       %T1120079031
     IF SC="~" THEN JUMP OUT TO XX ELSE JUMP OUT);                  %T1120079032
G(JUMP OUT TO XX); G:=TALLY; A:=SI; X:=DI; SI:=LOC G; DI:=B;        %T1120079033
DS:=WDS; SI:=A; DI:=X; TALLY:=1; G:=TALLY; TALLY:=0;                %T1120079034
2(36(IF SC="~" THEN JUMP OUT 2 TO XX ELSE                           %T1120079035
     IF SC="!" THEN JUMP OUT 2 TO Y ELSE                            %T1120079036
     IF SC="{" THEN JUMP OUT 2 TO Y ELSE DS:=CHR));                 %T1120079037
XX: DS:=LIT "~"; G:=TALLY; SI:=LOC G; DI:=E; DS:=WDS; END;          %T1120079038
SC(A,X,B,E);                                                        %T1120079040
IF B!0 THEN TWXOUTX(A,0,(B.[44:4]+32768|REAL(B.[40:4]=0)));         %T1120079041
TWXOUTX(X[0],72,T);                                                 %T1120079042
IF E!0 THEN TWXOUTX(A,0,(E.[44:4]+32768|REAL(E.[40:4]=0)));         %T1120079043
TWXOUT:=BRAAAAAK; BRAAAAAK:=FALSE; END;                             %T1120079044
PROCEDURE TWXIN(T,N,A); VALUE N; REAL T,N,A; COMMUNICATE(18);       %T1120079045
BOOLEAN PROCEDURE INTERRUPTED;                                      %T1120079046
BEGIN REAL G; TWXIN(G,9,INFO[U,0]); INTERRUPTED:=BOOLEAN(G); END;   %T1120079050
STREAM PROCEDURE PUTINFILETYPE(FIIL,TYPE); VALUE TYPE; %            %T0120079060
   BEGIN LOCAL SV; %                                                %T0120079062
   SI:=FIIL;   SI:=SI-24; DI:=LOC FIIL; DS:=WDS; %                  %T0120079064
   SI:=FIIL;14(SI:=SI+8); DI:=LOC FIIL; DS:=WDS; %                  %T0120079066
   DI:=FIIL;   DI:=DI+32; DI:=DI+6; %                               %T0120079068
   SI:=LOC TYPE; SI:=SI+7; DS:=CHR; %                               %T0120079070
   END; %                                                           %T0120079072
          FILL LOADFILE WITH    INFO [LOADLINK],USERID[USER]; %     %T0140043000
         SYNTAX[14] := 0; %           % IN CASE NOT UNDER GT MCP    %T0140044000
          SEARCH(LOADFILE,SYNTAX[*]); IF SYNTAX[0] LEQ 0 THEN BEGIN %T0140044500
          FILL LOADFILE WITH "REMOTE ",INFO[LOADLINK]; %            %T0140044600
          END; %                                                    %T0140046000
         IF IF SYNTAX[0] LEQ 0 THEN TRUE ELSE  %                    %T0140047000
         SYNTAX[14].[9:2] = 3 OR 0NEQ SYNTAX[14].[36:6]NEQ 30 THEN  %T0140048000
              INFO[FILELIMITS] .[1:1] := REAL(SYNTAX[0] GTR 0); %   %T0140052000
FILE IN HELP DISK SERIAL(2,10,30); %                                %T1140117100
          DEFINE TEMPDISKBUF = DISKBUF#; %                          %T1140119000
               FILL HELP WITH "WIPLS  ", "HELPFIL"; %               %T0140201000
              STARTUPMSG(TRUE); %        %                          %T1140209100
DEFINE STUP = 21#; %                                                %T1150045000
          GENCODE(STUP,0); %                                        %T1152155000
         IF HELPTABLE[0] = 0 THEN BEGIN %                           %T1153573100
          HELPLOADER; %                                             %T1153573200
         END; %                                                     %T1153581100
   WHILE (T:=SYNTAX[I:=I+1]).CLASSFIELD NEQ ENDR AND T.CLASSFIELD   %T0153636000
     NEQ IDLETTER DO; IF T.CLASSFIELD NEQ IDLETTER THEN BEGIN %-    %T0153637000
     FILL SAVER WITH T1, USERID[USER]; %                            %T0153649000
     IF T1 = 0 OR T1.[42:6] NEQ " " THEN %                          %T0153721000
               BEGIN SYNTAX[TEMPCODE]:= STUP|64; OK := TRUE; END;   %T1153883000
IF ABS(STACK[S]) GTR 549755813887 THEN GO INTERR ELSE %             %T1171076000
IF ABS(STACK[S]) GTR 549755813887 THEN GO INTERR ELSE %             %T1171078000
IF ABS(STACK[S]) GTR 549755813887 THEN GO INTERR ELSE %             %T1171080000
INTEGER STREAM PROCEDURE SBK(L); %                                  %T1071408000
BEGIN SI ~ L; SI ~ SI + 23; TALLY ~ 24; %                           %T1071408100
      24(IF SC ! " " THEN JUMP OUT; SI ~ SI - 1; TALLY ~ TALLY +63);%T1071408200
      SBK ~ TALLY; %                                                %T1071408400
END SBK; %                                                          %T1071408500
$VOIDT 71446000                                                     %T1071412000
                    WRITE(LINE[*],//,STACK[(S:=S-1)+1]);            %T1071444000
                    T3 ~ 0;   T2 ~ SBK(LINE); %                     %T1071445000
%                                                                   %T1180039000
%                                                                   %T1180041000
FILE IN  REMIN REMOTE (2,9); %                                      %T1180049000
$ VOIDT 80077001                                                    %T1180051000
OWN ARRAY DSTACK[0:STACKARRAYSIZE]; %                               %T1180099400
DEFINE STACK[STACK1,STACK2] = DSTACK[STACK2] #; %                   %T1180099500
OWN ARRAY DCODE[0:CODEMAXSIZE],DUSERARRAY[0:USERARRAYMAXSIZE]; %    %T1180100000
DEFINE CODE[CODE1,CODE2] =DCODE[CODE2] #, %                         %T1180100400
       USERARRAY[USERARRAY1,USERARRAY2] = DUSERARRAY[USERARRAY2] #; %T1180100500
COMMENT %                                                           %T1180100600
$VOID 80145000                                                      %T1180127000
; %                                                                 %T1180145000
$ VOIDT 80608001                                                    %T1180163500
          LABEL BREAK,CWRITE; %                                     %T1180164000
BOOLEAN STREAM PROCEDURE PILA(A,B); %                               %T1180165000
BEGIN LOCAL C;                                                      %T1180167000
SI := A; IF SC GTR "9" THEN TALLY := 1; 2(SI := SI + 36); %         %T1180169000
2(36(SI := SI - 1; IF SC NEQ " " THEN JUMP OUT 2 TO OK)); %         %T1180171000
OK: PILA := TALLY; %                                                %T1180173000
C := SI; DI := C; DI := DI + 1; DS := LIT "~"; %                    %T1180175000
SI := A; DI := B; DS := 09 WDS; %                                   %T1180177000
END PILA; %                                                         %T1180179000
DATACOM: %                                                          %T1180317000
IF BOOLEAN(RUNNER[U].[1:1]) THEN BEGIN %                            %T1180319000
WNEXT: %                                                            %T1180321000
IF TWXOUT(INFO[U,0],72,32768) THEN GO TO BREAK;                     %T1180323000
CWRITE: %                                                           %T1180325000
RUNNER[U] := ABS(RUNNER[U]); %                                      %T1180327000
EVENTS := EVENTS + 1; %                                             %T1180329000
IF RUNNER[U].[36:12] NEQ 0 THEN IDLE := TRUE AND NOT ACTIVE := TRUE;%T1180331000
GO TO RESTORE; %                                                    %T1180333000
BREAK: %                                                            %T1180335000
IF RUNNER[U].[36:12] = "I" THEN BEGIN % SPECIAL ACTION ON LOADING   %T1180337000
CLOSE(LOADFILE); LOADINTERLOCK := -1; INFO[U,LOADLIST] := 0; %      %T1180339000
END; %                                                              %T1180341000
RUNNER[U] := 0; %                                                   %T1180343000
EVENTS := EVENTS + 1; %                                             %T1180345000
%                                                                   %T1180347000
END; %                                                              %T1180359000
RESTORE: %                                                          %T1180411000
IF RUNNER[U].[36:12] NEQ 0 THEN IF   %                              %T1180413050
    IF REAL(BOOLEAN(EVENTS) AND BOOLEAN(7)) = 0 THEN   %            %T1180413051
    INTERRUPTED ELSE FALSE THEN GO BREAK ELSE %                     %T1180413052
GO XIT; %                                                           %T1180413060
DO READ(REMIN,9,INFO[U,*]) UNTIL NOT %                              %T1180415000
   PILA(INFO[U,*],SYNTAX[*]); %                                     %T1180427000
DCIO := TRUE AND NOT IDLE := ACTIVE := FALSE; %                     %T1180429000
EVENTS := EVENTS + 1; %                                             %T1180431000
GO TO XIT; %                                                        %T1180433000
         IF SC = "O" THEN DS ~ 24 LIT "NOT LOADED(INVALID TYPE)";   %T0180642000
          IF SC = "B" THEN DS ~ 52 LIT %                            %T1180645000
           "TYPE HELP IF YOU HAVE QUESTIONS  (WIPL VERSION 1.5)~"; %%T1180647000
%                                                                   %T1180663000
$ VOIDT 80693001                                                    %T1180687000
STREAM PROCEDURE PUBLICSAVE(D,N,M); VALUE N,M; %                    %T0180709000
          DI~D; DS~09LIT"CC PUBLIC"; SI~LOC N; SI~SI+1; DS~7CHR;    %T0180713000
DS~LIT"/"; SI~SI+1    ; DS ~ 7 CHR ; DS ~ 6 LIT ";END.~" ; %        %T0180715000
%                                                                   %T1180739000
    LABEL OK,EOF; %                                                 %T1180741000
STREAM PROCEDURE FORMATERRORS(S,S1,D); %                            %T1180741001
BEGIN LOCAL T; %                                                    %T1180741002
     SI ~ S1; SI ~ SI -1; %                                         %T1180741003
     63(IF SC ! " " THEN JUMP OUT ELSE SI ~ SI -1); %               %T1180741004
     SI ~ SI +1; T ~ SI; DI ~ T; DS ~ LIT "~"; %                    %T1180741005
     SI ~ S; 15(IF SC = "M" THEN JUMP OUT ELSE SI ~ SI +1); %       %T1180741006
     SI ~ SI +2; DI ~ D; DS ~ 63 CHR; %                             %T1180741007
END FORMATTING ERRORS; %                                            %T1180741008
    READ(ERRF[ERRNUMBER],10,LISTBUF[*])[EOF:EOF]; %                 %T1180742000
    FORMATERRORS(LISTBUF[0],LISTBUF[9],INFO[USER,IOBUF]); %         %T1180743000
    GO OK; %                                                        %T1180744000
    EOF: CLOSE(ERRF); %                                             %T1180745000
    INFO[USER,IOBUF] ~ "ERROR.~"; %                                 %T1180746000
    OK: %                                                           %T1180747000
DEFINE CHARGETHEUSER = #; %                                         %T1180753000
%                                                                   %T1180921000
         IF DEBUG THEN WRITE(PR[DBL],*,RUNNER[0]); %                %T1180927000
%                                                                   %T1180929000
                   IF INFO[USER,FILELIMITS]=0 THEN MSG( %           %T0181093000
                      INFO[USER,FILELIMITS].[1:1] + "N", %          %T0181094000
$ VOIDT 81115001                                                    %T1181107000
                   FILL SAVER WITH INFO[USER,FILENAME],USERID[USER];%T0181231000
                             PUTINFILETYPE(SAVER,30); % WIPLS TYPE  %T0181268000
                             FILENAME],USERID[USER]); %             %T0181279000
$ VOIDT 81307001                                                    %T1181295000
%                                                                   %T1181312500
                   RUNNER[USER] := USERID[USER] := 0; %             %T1181312700
                   ACTIVE := FALSE; %                               %T1181312900
$ VOIDT 81341001                                                    %T1181313000
%                                                                   %T1181351000
%                                                                   %T1181353000
WRITER:   CARRIAGE(INFO[USER,IOBUF],0); %                           %T1181369000
ENDLOOP: %                                                          %T1181377000
%                                                                   %T1181379000
$ VOIDT 90013000                                                    %T1190002000
         LABEL BREAK;    ARRAY REMOT[0:9]; %                        %T1190016000
IF TF THEN %                                                        %T1190017000
WRITE(REMOT[*   ],<"PLEASE WAIT.{!~">) ELSE                         %T1190019000
WRITE(REMOT[*   ],<"SOMEONE ELSE IS USING WIPL UNDER YOUR USER-CODE"%T1190021000
".{!~">);                                                           %T1190023000
IF TWXOUT(REMOT[0],72,0) THEN BREAK:; %                             %T1190024000
MONITOR EXPOVR,INTOVR,ZERO,INDEX ~ INDX,FLAG ~ FLG; %               %T1190053100
LABEL OOPS; %                                                       %T1190053200
EXPOVR~INTOVR~INDX~FLG ~ZERO~OOPS; %                                %T1190063100
FILL DSK WITH "WIPLSAV",        USERID[0] := TIME(-1),*,*,*,10; %   %T1190333000
IF INFO[0,6] GEQ 1 OR INFO[0,0] = 0 THEN BEGIN %                    %T1190337000
$ VOIDT 90349001                                                    %T1190341000
OOPS: %                                                             %T1190353100
$ VOIDT 90379001                                                    %T1190365000
FILL MAKER WITH "WIPLSAV",USERID[0],*,*,*,10; %                     %T1190383000
PUTINFILETYPE(MAKER,30);                                            %T1190418000
$ VOIDT 90465001                                                    %T1190421000
     FILL LOADFILE WITH "WIPLS  ","ERRORS "; %                      %T0190433000
$VOIDT 90487001                                                     %T1190473000
RUNNER[USER] := "B";                                                %T1190581100
ACTIVE := TRUE; %                                                   %T1190581200
IDLE := FALSE; %                                                    %T1190581300
