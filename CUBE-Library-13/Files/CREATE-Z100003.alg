          BEGIN                                                                 
COMMENT        PROGRAM CREATE/FILE, TO CREATE A DISK FILE LIKE                  
               TEXT/EDITOR DOES BUT USING LESS MEMORY ETC;                      
COMMENT        CUBE LIBRARY NUMBER Z100003,THIS VERSION DATED 2-8-68.           
               WRITTEN BY DONNA WILLIAMS & W.H.EICHELBERGER,                    
               UNIVERSITY OF DENVER;                                            
COMMENT                                                                         
FILE           DF1 DISK SERIAL "FIRST""SECOND"(1,10,30),                        
               DF2 DISK SERIAL [20:6] "FIRST""SECOND"(1,10,30,SAVE 99);         
FILE           DF1 DISK SERIAL "FIRST""SECOND"(1,10,150),                       
               DF2 DISK SERIAL [20:15]"FIRST""SECOND"(1,10,150,SAVE 99);        
SWITCH FILE    DF~DF1,DF2;                                                      
ALPHA FILE IN  TTIN 14(2,8);                                                    
ALPHA FILE OUT TOUT 14(2,8);                                                    
SAVE ARRAY     A,DATA[0:9];                                                     
SAVE ARRAY     READIN,FREE[0:7];                                                
SAVE ARRAY     DICT[0:3];                                                       
ARRAY          DIC[0:1];                                                        
INTEGER        JJ,KK;                                                           
BOOLEAN        ALFASEQ;                                                         
INTEGER        F;                                                               
ALPHA          ND1,ND2;                                                         
INTEGER        N,J,K,I,L,M,P,STA,R;                                             
ALPHA          ID1,ID2;                                                         
BOOLEAN        RLFG,ILFG,SRTFG,THRFG,PLFG;                                      
LABEL          REED,DONE,REED1,THRU;                                            
LABEL             TC,RL,PL,OK,CS,RS,IL,DL,GA,ST,ERR;                            
SWITCH         SW~TC,RL,PL,OK,CS,RS,IL,DL,GA,ST,ERR;                            
DEFINE      READSTA=READ(TTIN(STA),8,READIN[*]); WRITE(TOUT(STA),FCR)#,         
               ERROP=11#;                                                       
FORMAT         FNO (X8,"IMPROPER RESPONSE. TRY AGAIN{!~"),                      
               FCR(X8,"{!~"),                                                   
               FDONE(X8,"DONE{!~"),                                             
               FSRT(X8,"FILE BEING SORTED AND RESEQUENCED. WAIT{!~"),           
               FST(X8,"{!PROGRAM TERMINATED{!~"),                               
               FOT(X8,I8,  "~"),                                                
               ILL(X8,"ILLEGAL OPERATION CODE.{!~"),                            
               NOPL(X8,"NO SUCH LINE NUMBER.{!~"),                              
               FCF4(X8,"THIS OPERATION LEGAL ONLY AFTER RL.{!~"),               
               NTFND(X8,"LINE ",I8," NOT IN FILE.{!~");                         
STREAM PROCEDURE MOV(F,T,N,M);                                                  
VALUE          N,M;                                                             
          BEGIN                                                                 
               SI~F; DI~T; DS~ N CHR;  M(DS~ 1 LIT " ");                        
          END  MOV;                                                             
BOOLEAN STREAM PROCEDURE CMD(A);                                                
          BEGIN                                                                 
               SI~A;                                                            
               IF SC = "*" THEN TALLY~1;                                        
               CMD~TALLY;                                                       
          END  CMD;                                                             
PROCEDURE      WRITESEQ(I);                                                     
INTEGER        I;                                                               
          BEGIN                                                                 
               WRITE(TOUT(STA),FOT,I);                                          
          READ SEEK(TTIN(STA));                                                 
          END  WRITESEQ;                                                        
STREAM PROCEDURE BCV(A,B);                                                      
          BEGIN                                                                 
               SI~A; DI~B; DS~8 OCT;                                            
          END  BCV;                                                             
STREAM PROCEDURE  OCV(OCTL,CHRS);                                               
          BEGIN                                                                 
               SI~OCTL;  DI~CHRS; DS~8 DEC;                                     
          END  OCV;                                                             
STREAM PROCEDURE MOVE(F,T,N);                                                   
VALUE          N;                                                               
          BEGIN                                                                 
               SI~F;  SI~SI+3;                                                  
               DI~T;                                                            
               DS~N  CHR;  DS~2 LIT ",,";                                       
          END  MOVE;                                                            
PROCEDURE      TYPE;                                                            
          BEGIN                                                                 
INTEGER STREAM PROCEDURE NCH(A);                                                
          BEGIN                                                                 
               SI~A; SI~SI+50;   SI~SI+21;                                      
               63(IF SC!" " THEN JUMP OUT; TALLY~TALLY+1; SI~SI-1);             
               NCH~TALLY;                                                       
          END  NCH;                                                             
STREAM PROCEDURE MOVE1(F,T,N,ND);                                               
VALUE          N;                                                               
          BEGIN                                                                 
               SI~F; DI~T; SI~SI+50; SI~SI+22;DS~8 CHR;SI~F;                    
               DS~N CHR;                                                        
               SI~ND;  SI~SI+5;  DS~3 CHR;                                      
          END  MOVE1;                                                           
STREAM PROCEDURE MOVE2(F,T,N,ND,SKP);                                           
VALUE          N,SKP;                                                           
          BEGIN                                                                 
               SI~F; SI~SI+SKP;                                                 
                     DI~T;  DS~N CHR;   SI~ND; SI~SI+5;  DS~ 3 CHR;             
          END  MOVE2;                                                           
INTEGER        I;                                                               
               I~NCH(DATA);                                                     
               IF I>26 THEN                                                     
          BEGIN                                                                 
               MOVE1(DATA,A[1],72-I,ND1);                                       
               WRITE(TOUT(STA),8,A[*]);                                         
          END  ELSE IF I>9 THEN                                                 
          BEGIN                                                                 
               MOVE1(DATA,A[1],32,ND2);                                         
               WRITE(TOUT(STA),7,A[*]);                                         
               MOVE2(DATA[4],A[1],40-I,ND1,0);                                  
               WRITE(TOUT(STA),6,A[*]);                                         
          END  ELSE                                                             
          BEGIN                                                                 
               MOVE1(DATA,A[1],32,ND2);                                         
               WRITE(TOUT(STA),7,A[*]);                                         
               MOVE2(DATA[4],A[1],30,ND1,0);                                    
               WRITE(TOUT(STA),6,A[*]);                                         
               MOVE2(DATA[7],A[1],10-I,ND1,6);                                  
               WRITE(TOUT(STA),4,A[*]);                                         
          END;                                                                  
          END  TYPE;                                                            
INTEGER STREAM PROCEDURE FINDGP(A);                                             
          BEGIN                                                                 
LABEL          FOUND;                                                           
               SI~A;  56(IF SC ="~" THEN JUMP OUT 1 TO FOUND;  SI~SI+1;         
                 TALLY~TALLY+1);                                                
FOUND:         FINDGP~TALLY;                                                    
          END  FINDGP;                                                          
INTEGER STREAM PROCEDURE FINDOP(S,D);                                           
          BEGIN                                                                 
LABEL          HERE;                                                            
LOCAL          ST1;                                                             
               SI~S; SI~SI+1;  ST1~SI;  DI~D;                                   
            ERROP(TALLY~TALLY+1;  IF 2 SC=DC THEN JUMP OUT 1 TO HERE;           
                  SI~ST1;);                                                     
HERE:          FINDOP~TALLY;                                                    
          END  FINDOP;                                                          
BOOLEAN STREAM PROCEDURE PCT  (A,N);                                            
VALUE          N;                                                               
          BEGIN                                                                 
               SI~A; SI~SI+N; TALLY~0; SI~SI-1;                                 
               IF SC = "%" THEN TALLY~1;                                        
               PCT~TALLY;                                                       
          END  PCT;                                                             
PROCEDURE      TEACH;                                                           
          BEGIN                                                                 
SWITCH FORMAT  CF2~                                                             
               (X8,"OPERATION CODES ARE:{!~"),                                  
               (X8,"*GO - STARTS PROGRAM.{!~"),                                 
               (X8,"*ST - STOPS PROGRAM - FILE IS ALREADY SAVED.{!~"),          
               (X8,"*PL X OR *PL X,Y - PRINTS LINES X THRU Y.{!~"),             
               (X8,"*IL X - INSERTS A LINE NUMBER X.{!~"),                      
               (X8,"*DL X OR *DL X,Y - DELETES LINES X THRU Y.{!~"),            
               (X8,"*RL X - RECALLS LINE X, LISTS, REPEATS NUMBER.{!~"),        
               (X8,"*OK - RE-STORES LINE UNCHANGED.{!~"),                       
               (X8,"*CS Y - RE-STORES WITH SEQ. NO. CHANGED TO Y.{!~"),         
               (X8,"*RS - SORTS AND RESEQUENCES BY 100.{!~"),                   
               (X8,"*TC - LISTS COMMANDS AVAILABLE.{!~"),                       
               (X8," DO YOU NEED MORE HELP. TYPE YES OR *GO.{!~");              
SWITCH FORMAT  CF2A~                                                            
          (X8,"THIS PROGRAM CREATES OR CHANGES A CARD-IMAGE DISK{!~"),          
          (X8,"FILE. IT ASKS YOU FOR FILE NAME. FILE MAY BE UP TO{!~"),         
          (X8,"300 LINES. SEQUENCE NUMBERS ARE IN COLS 73-80 BUT{!~"),          
          (X8,"PRINT AT LEFT.{!~"),                                             
          (X8,"THE PROGRAM NORMALLY SUPPLIES THE NEXT SEQUENCE{!~"),            
          (X8,"NUMBER AND YOU MAY TYPE THE DATA TO BE STORED AT{!~"),           
          (X8,"THAT NUMBER. YOU MAY INSERT A LINE WITH THE *IL{!~"),            
          (X8,"COMMAND OR DELETE A LINE OR SEQUENCE OF LINES WITH{!~"),         
          (X8,"*DL. *PL LETS YOU PRINT A LINE OR SEQUENCE OF{!~"),              
          (X8,"LINES, OR IF X & Y ARE NOT SUPPLIED IT PRINTS THE{!~"),          
          (X8,"ENTIRE FILE.{!~"),                                               
          (X8,"WITH *RL YOU MAY RECALL A LINE FOR CHANGING. *OK{!~"),           
          (X8,"WILL RE-STORE IT UNCHANGED, & *CS Y WILL RE-STORE{!~"),          
          (X8,"IT WITH SEQUENCE NUMBER CHANGED TO Y.{!~"),                      
          (X8,"A % IMMEDIATELY BEFORE THE LEFT ARROW ERASES A LINE{!~"),        
          (X8,"AND LETS YOU START IT OVER.{!~"),                                
          (X8,"IF YOU WISH A FILE CREATED BY YOU TO BE SAVED AFTER{!~"),        
          (X8,"5 PM, YOU MUST FILE A LIBRARY MAINTENANCE FORM{!~"),             
          (X8,"IN THE COMPUTING CENTER{!!~");                                   
INTEGER        I;                                                               
LABEL          THRU,MH,TC1;                                                     
               FOR J~0 STEP 1 UNTIL 11 DO WRITE(TOUT(STA),CF2[J]);              
TC1:           READSTA;                                                         
               I~FINDOP(READIN[1],DIC);                                         
               IF I=2 THEN GO TO THRU ELSE IF I=5 THEN GO TO MH;                
               WRITE(TOUT(STA),FNO);                                            
               WRITE(TOUT(STA),CF2[11]);                                        
               GO TO TC1;                                                       
MH:            FOR J~0 STEP 1 UNTIL 18 DO WRITE(TOUT(STA),CF2A[J]);             
THRU:     END  TEACH;                                                           
PROCEDURE      PROGSTART;                                                       
          BEGIN                                                                 
STREAM PROCEDURE FILLTHE(DICT);                                                 
          BEGIN                                                                 
               DI~DICT;                                                         
               DS~2 LIT "TC"; % LISTS COMMANDS                                  
               DS~2 LIT "RL"; % RECALLS A LINE                                  
               DS~2 LIT "PL"; % PRINTS LINES                                    
               DS~2 LIT "OK"; % RESTORES LINE                                   
               DS~2 LIT "CS"; % RESTORES LINE, NEW SEQUENCE                     
               DS~2 LIT "RS"; % SORTS AND RESEQUENCES BY 100                    
               DS~2 LIT "IL"; % INSERTS NEW LINE NUMBER                         
               DS~2 LIT "DL"; % DELETES LINES                                   
               DS~2 LIT "GO"; % STARTS PROGRAM                                  
               DS~2 LIT "ST"; % STOPS PROGRAM                                   
          END  OF FILLTHEDICT;                                                  
STREAM PROCEDURE FILLITTLE(DIC);                                                
          BEGIN                                                                 
               DI~DIC;                                                          
               DS~2 LIT "TC"; % LISTS COMMANDS                                  
               DS~2 LIT "GO"; % STARTS PROGRAM                                  
               DS~2 LIT "EW"; % NEW FILE                                        
               DS~2 LIT "LD"; % OLD FILE                                        
               DS~2 LIT "ES"; % MORE HELP                                       
               DS~2 LIT "ST"; %STOP                                             
          END  FILLITTLEDIC;                                                    
BOOLEAN STREAM PROCEDURE ALF(A);                                                
          BEGIN                                                                 
COMMENT        THIS IS SUPPOSED TO RETURN TRUE IF EITHER OF THE                 
               FIRST TWO CHARACTERS IS ALPHABETIC;                              
               SI~A;                                                            
               2(IF SB THEN TALLY~1; SKIP 1 SB);                                
               SKIP 4 SB;                                                       
               2(IF SB THEN TALLY~1; SKIP 1 SB);                                
               ALF~TALLY;                                                       
          END  ALF;                                                             
STREAM PROCEDURE  MVID(F,T,N);                                                  
VALUE          N;                                                               
          BEGIN                                                                 
               SI~F;DI~T;DI~DI+1;                                               
               DS~N CHR;                                                        
          END  MVID;                                                            
FORMAT         CF0(X8,"{!YOU ARE USING CREATE/FILE, VERSION 2A{!~"),            
               CF1(X8,"IF YOU NEED HELP TYPE *TC ELSE TYPE *GO{!~"),            
               CF3(X8,"{!NEW OR OLD.{!~"),                                      
               CF4(X8,"{!FIRST NAME OF FILE:  ~"),                              
               CF5(X8,"{!SECOND NAME OF FILE: ~"),                              
               CF6(X8,"{!FILE ",A1,A6,"/",A1,A6," FOUND.LAST LINE:{!~"),        
               CF7(X8,"{!FILE ",A1,A6,"/",A1,A6," BEING CREATED AND SAVE        
D{!~"),        CF8(X8,"{!FILE ",A1,A6,"/",A1,A6," NOT ON DISK{!~"),             
               CF9(X8,"{!FILE ",A1,A6,"/",A1,A6," ALREADY ON DISK{!~"),         
               CF10(X8,"PLEASE SUPPLY A NEW NAME{!~");                          
LABEL          PS1,TC,LGO,LGO1,OLDF,NEWF,READY;                                 
               FILLITTLE(DIC);                                                  
               ALFASEQ~SRTFG~FALSE;                                             
               STA~STATUS(READIN[*]);                                           
               STA~0&READIN[0][9:9:9];                                          
               FILLTHE(DICT);                                                   
               ND1~"{!~";                                                       
               ND2~"~  ";                                                       
               WRITE(TOUT(STA),CF0);                                            
PS1:           WRITE(TOUT(STA),CF1);                                            
               READSTA;                                                         
               I~FINDOP(READIN[1],DIC);                                         
               IF I=2 THEN GO TO LGO;                                           
               IF I=1 THEN GO TO TC;                                            
               IF I=6 THEN GO TO DONE;                                          
               WRITE(TOUT(STA),FNO);                                            
               GO TO PS1;                                                       
TC:            TEACH;                                                           
LGO:                                                                            
               ID1~ID2~"      ";                                                
               WRITE(TOUT(STA),CF4);                                            
               READSTA;                                                         
               I~FINDGP(READIN[1]);   IF I>7 THEN I~7;                          
               MVID(READIN[1],ID1,I);                                           
               WRITE(TOUT(STA),CF5);    READSTA;    I~FINDGP(READIN[1]);        
               IF I>7 THEN I~7;                                                 
               MVID(READIN[1],ID2,I);                                           
LGO1:          WRITE(TOUT(STA),CF3);                                            
               READSTA;                                                         
               I~FINDOP(READIN[1],DIC);                                         
               IF I=3 THEN GO TO NEWF;                                          
               IF I=4 THEN GO TO OLDF;                                          
               IF I=6 THEN GO TO DONE;                                          
               WRITE(TOUT(STA),FNO);                                            
               GO TO LGO1;                                                      
OLDF:          F~0;                                                             
               FILL DF1 WITH ID1,ID2;                                           
               SEARCH(DF1,A[*]);                                                
               IF A[0]=-1 OR A[0] = 0  THEN  BEGIN                              
                 WRITE(TOUT(STA),CF8,ID1.[6:6],ID1,ID2.[6:6],ID2);              
                 WRITE(TOUT(STA),CF10);                                         
                 GO TO LGO;                                                     
               END;                                                             
                 WRITE(TOUT(STA),CF6,ID1.[6:6],ID1,ID2.[6:6],ID2);              
               N~A[5];                                                          
               READ(DF[F][N],10,DATA[*]);                                       
               TYPE;                                                            
               ALFASEQ~ALF(DATA[9]);                                            
               IF ALFASEQ THEN GO TO RS;                                        
               BCV(DATA[9],I);                                                  
               I~I+100;                                                         
               GO TO READY;                                                     
NEWF:          F~1;                                                             
               FILL DF2 WITH ID1,ID2;                                           
               SEARCH(DF2,A[*]);                                                
               IF A[0]! -1 THEN  BEGIN                                          
                 WRITE(TOUT(STA),CF9,ID1.[6:6],ID1,ID2.[6:6],ID2);              
                 WRITE(TOUT(STA),CF10);                                         
                 GO TO LGO;                                                     
               END;                                                             
               I~100;                                                           
               OCV(I,A[9]);                                                     
               WRITE(DF[F][0],10,A[*]);                                         
               LOCK(DF2,RELEASE);                                               
               FILL DF1 WITH ID1,ID2;                                           
               F~0;                                                             
               N~-1;                                                            
                 WRITE(TOUT(STA),CF7,ID1.[6:6],ID1,ID2.[6:6],ID2);              
READY:    END  PROGSTART;                                                       
               PROGSTART;                                                       
               OCV(I,A[9]);                                                     
REED: OK: GA:  WRITESEQ(I);                                                     
               ILFG~RLFG~FALSE;                                                 
REED1:         READ(TTIN(STA),8,READIN[*]);                                     
               IF CMD(READIN[1]) THEN                                           
          BEGIN                                                                 
               WRITE(TOUT(STA),FCR);                                            
               P~FINDOP(READIN[1],DICT);                                        
               GO TO SW[P];                                                     
PL:            K~L~0;                                                           
               MOVE(READIN[1],FREE,FINDGP(READIN[1])-3);                        
               READ(FREE[*],/,K,L);                                             
               IF L=0 THEN L~K;                                                 
               IF L=0 THEN L~399999999999;                                      
               FOR J~0 STEP 1 UNTIL N DO                                        
          BEGIN                                                                 
               READ(DF[F] [J],10,DATA[*]);                                      
               IF DATA[9]!400000000000 THEN                                     
          BEGIN                                                                 
               BCV(DATA[9],M);                                                  
               IF M}K AND M{L THEN                                              
          BEGIN                                                                 
               TYPE;                                                            
               PLFG~TRUE;                                                       
          END;                                                                  
          END;                                                                  
          END;                                                                  
               IF NOT PLFG THEN WRITE(TOUT(STA),NOPL);                          
               PLFG~FALSE;                                                      
               GO TO REED;                                                      
ERR:           WRITE(TOUT(STA),ILL);                                            
               GO TO REED;                                                      
RL:                                                                             
          BEGIN                                                                 
LABEL          NOTFND,FNDL;                                                     
               MOVE(READIN[1],FREE,FINDGP(READIN[1])-3);                        
               READ(FREE[*],/,K);                                               
               J~0;                                                             
               WHILE TRUE DO                                                    
          BEGIN                                                                 
               READ(DF[F][J],10,DATA[*])[NOTFND];                               
               BCV(DATA[9],L);                                                  
               IF L=K THEN GO TO FNDL;                                          
               J~J+1;                                                           
          END;                                                                  
NOTFND:        WRITE(TOUT(STA),NTFND,K);                                        
               GO TO REED;                                                      
FNDL:          TYPE;                                                            
               RLFG~TRUE;                                                       
               R~J;                                                             
               WRITESEQ(K);                                                     
          END;                                                                  
               GO TO REED1;                                                     
CS:            IF NOT RLFG THEN                                                 
                BEGIN  WRITE(TOUT(STA),FCF4);                                   
                 GO TO REED;  END;                                              
               MOVE(READIN[1],FREE,FINDGP(READIN[1])-3);                        
               READ(FREE[*],/,K);                                               
               SRTFG~TRUE;                                                      
               OCV(K,DATA[9]);                                                  
               WRITE(DF[F][J],10,DATA[*]);                                      
               WRITE(TOUT(STA),FDONE);                                          
               GO TO REED;                                                      
IL:                                                                             
               MOVE(READIN[1],FREE,FINDGP(READIN[1])-3);                        
               READ(FREE[*],/,K);                                               
               FOR J~0 STEP 1 UNTIL N DO                                        
          BEGIN                                                                 
               READ(DF[F][J],10,DATA[*]);                                       
               BCV(DATA[9],L);                                                  
               IF L=K THEN BEGIN                                                
                TYPE;                                                           
                GO TO REED; END;                                                
          END;                                                                  
               ILFG~TRUE;                                                       
               WRITESEQ(K);                                                     
               SRTFG~TRUE;                                                      
               OCV(K,A[9]);                                                     
               GO TO REED1;                                                     
TC:            TEACH;                                                           
               GO TO REED;                                                      
DL:                                                                             
          BEGIN                                                                 
LABEL          DUMMY;                                                           
               K~L~0;                                                           
               MOVE(READIN[1],FREE,FINDGP(READIN[1])-3);                        
               READ(FREE[*],/,K,L);                                             
               IF L=0 THEN L~K;                                                 
               FOR J~0 STEP 1 UNTIL N DO                                        
          BEGIN                                                                 
               READ(DF[F][J],10,DATA[*]);                                       
               BCV(DATA[9],M);                                                  
               IF  M}K AND M{L THEN                                             
                BEGIN DATA[9]~400000000000;                                     
                WRITE(DF[F][J],10,DATA[*]); END;                                
          END;                                                                  
               SRTFG~TRUE;                                                      
               WRITE(TOUT(STA),FDONE);                                          
          END;                                                                  
               GO TO REED;                                                      
RS:       BEGIN                                                                 
PROCEDURE      HV(A);                                                           
ARRAY          A[0];                                                            
               A[9]~500000000000;                                               
BOOLEAN PROCEDURE CMPR(A,B);                                                    
ARRAY          A,B[0];                                                          
              CMPR~IF A[9]{B[9] THEN TRUE ELSE FALSE;                           
BOOLEAN PROCEDURE IP(A);                                                        
ARRAY          A[0];                                                            
          BEGIN                                                                 
LABEL          EOF,THRU,LA;                                                     
LA:            READ(DF[F][J],10,A[*])[EOF];                                     
               J~J+1;                                                           
               IF NOT ALFASEQ THEN                                              
               IF A[9]=400000000000 THEN GO TO LA;                              
               GO TO THRU;                                                      
EOF:           IP~TRUE;                                                         
               CLOSE(DF[F],PURGE);                                              
               FILL DF2 WITH ID1,ID2;                                           
               F~1;                                                             
               J~0;                                                             
THRU:     END  IP;                                                              
PROCEDURE      OP(B,A);                                                         
VALUE          B;                                                               
BOOLEAN        B;                                                               
ARRAY          A[0];                                                            
               IF B THEN                                                        
          BEGIN                                                                 
               LOCK(DF2,RELEASE);                                               
               FILL DF1 WITH ID1,ID2;                                           
               F~0;                                                             
          END  ELSE                                                             
          BEGIN                                                                 
               WRITE(DF2[J],10,A[*]);                                           
               J~J+1;                                                           
          END;                                                                  
               J~0;                                                             
               WRITE(TOUT(STA),FSRT);                                           
               SORT(OP,IP,0,HV,CMPR,10,1000,20000);                             
               N~J-1;                                                           
               I~100;                                                           
               FOR J~0 STEP 1 UNTIL N DO                                        
          BEGIN                                                                 
               READ(DF[F][J],10,DATA[*]);                                       
               OCV(I,DATA[9]);                                                  
               WRITE(DF[F][J],10,DATA[*]);                                      
               I~I+100;                                                         
          END;                                                                  
               CLOSE(DF[F]);                                                    
               ALFASEQ~SRTFG~FALSE;                                             
               WRITE(TOUT(STA),FDONE);                                          
          END  OF SORT BLOCK;                                                   
               IF THRFG THEN GO TO DONE;                                        
               OCV(I,A[9]);                                                     
               GO TO REED;                                                      
          END OF COMMAND BLOCK;                                                 
               J~FINDGP(READIN[1]);                                             
               JJ~0;                                                            
               KK~72-J;                                                         
               IF J=56 THEN BEGIN                                               
                MOV(READIN[1],A,56,0);                                          
                READ(TTIN(STA),8,READIN[*]);                                    
                J~FINDGP(READIN[1]);                                            
                IF J>16 THEN J~16;                                              
                JJ~7;                                                           
                KK~16-J;         END;                                           
               WRITE(TOUT(STA),FCR);                                            
               IF PCT  (READIN[1],J) THEN                                       
                BEGIN IF ILFG OR RLFG THEN BEGIN                                
                 WRITESEQ(K);                                                   
                 GO TO REED1  END ELSE GO TO REED END;                          
               IF KK{63 THEN                                                    
               MOV (READIN[1],A[JJ],J,KK)                                       
               ELSE BEGIN                                                       
                MOV(READIN[1],A,J,KK-16);                                       
                MOV(READIN[1],A[7],0,16); END;                                  
               IF RLFG THEN                                                     
          BEGIN                                                                 
               A[9]~DATA[9];                                                    
               WRITE(DF[F][R],10,A[*]);                                         
          END  ELSE                                                             
          BEGIN                                                                 
               N~N+1;                                                           
               WRITE(DF[F][N],10,A[*]);                                         
               CLOSE(DF[F]);                                                    
               IF NOT ILFG THEN I~I+100;                                        
          END;                                                                  
               OCV(I,A[9]);                                                     
               GO TO REED;                                                      
THRU: ST:      THRFG~TRUE;                                                      
               IF SRTFG THEN GO TO RS;                                          
DONE:                                                                           
               WRITE(TOUT(STA),FST);                                            
          END.                                                                  
