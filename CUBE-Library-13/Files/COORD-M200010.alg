          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS M200010.  THE PROGRAM NAME IS     00000200
               "COORD/TTY".  THIS VERSION DATED 5/3/68;                 00000300
ALPHA FILE IN  TTIN 14(2,8);                                            00000400
ALPHA FILE OUT TTOUT 14(2,8);                                           00000500
ARRAY          Z[0:7];                                                  00000600
INTEGER        GP,S;                                                    00000700
LABEL          LRTP,LPTR,DUNN,L1,L2,MORE;                               00000800
FORMAT         F1(X8,"{!TYPE RTP FOR RECT TO POLAR{!~"),                00000900
               F2(X8,"TYPE PTR FOR POLAR TO RECT{!~"),                  00001000
               F2A(X8,"AT THE END OF EACH SET TYPE RTP OR PTR!{~"),     00001100
               F2B(X8,"OR TYPE NO IF YOU WISH TO QUIT{!~"),             00001200
               FA(X8,"{!A= ~"),                                         00001300
               FB(X8,"{!B= ~"),                                         00001400
               FC(X8,"{!C= ~"),                                         00001500
               FT(X8,"{!ANGLE= ~"),                                     00001600
               F3(X8,A3),                                               00001700
               F4(X8,"{!YOU DID NOT ENTER PTR OF RTP. YOU WILL BE~"),   00001800
               F5(X8,"{!GIVEN ONE MORE CHANCE{!~"),                     00001900
               F6(X8,R*.0),                                             00002000
               F7(X8,"{!C= ",R20.10,"  ANGLE= ",R20.10,"{!~"),          00002100
               F8(X8,"{!A= ",R20.10,"  B= ",R20.10,"{!~");              00002200
ALPHA          WHICH;                                                   00002300
BOOLEAN        NOGO;                                                    00002400
DEFINE         RT=READ(TTIN(S)#, WT=WRITE(TTOUT(S)#;                    00002500
REAL           A,B,C,T;                                                 00002600
INTEGER STREAM PROCEDURE FINDGP(B);                                     00002700
          BEGIN                                                         00002800
LABEL          HERE;                                                    00002900
               SI~B; 20(IF SC="~" THEN JUMP OUT 1 TO HERE;              00003000
               SI~SI+1; TALLY~TALLY+1);                                 00003100
HERE:          FINDGP~TALLY;                                            00003200
          END;                                                          00003300
PROCEDURE      READIT(V,FF);                                            00003400
REAL           V;                                                       00003500
FORMAT         FF;                                                      00003600
          BEGIN                                                         00003700
               WT,FF);   RT,8,Z[*]);                                    00003800
               GP~FINDGP(Z[1]);                                         00003900
               READ(Z[*],F6,GP,V);                                      00004000
          END;                                                          00004100
               NOGO~FALSE;                                              00004200
                 S~STATUS(Z[*]);                                        00004300
                 S.[9:9]~Z[0].[9:9];                                    00004400
L1:            WT,F1);  WT,F2);  WT,F2A); WT,F2B);                      00004500
L2:            RT,F3,WHICH);                                            00004600
               IF WHICH="RTP" THEN GO TO LRTP;                          00004700
               IF WHICH="PTR" THEN GO TO LPTR;                          00004800
               IF WHICH.[36:12]="NO" THEN GO TO DUNN;                   00004900
               IF NOGO THEN GO TO DUNN;                                 00005000
               NOGO~TRUE;                                               00005100
               WT,F4);  WT,F5);                                         00005200
               GO TO L1;                                                00005300
LRTP:          READIT(A,FA);  READIT(B,FB);                             00005400
               C~SQRT(A*2+B*2);                                         00005500
               T~ARCTAN(B/A)|57.29578;                                  00005600
               IF A<0 AND B<0 THEN T~T-180;                             00005700
               IF A<0 AND B>0 THEN T~T+180;                             00005800
               WT,F7,C,T);                                              00005900
               GO TO MORE;                                              00006000
LPTR:          READIT(C,FC); READIT(T,FT);                              00006100
               T~T|0.017453;                                            00006200
          A~C|COS(T);                                                   00006300
               B~C|SIN(T);                                              00006400
               WT,F8,A,B);                                              00006500
MORE:          GO TO L2;                                                00006600
DUNN:     END.                                                          00006700
