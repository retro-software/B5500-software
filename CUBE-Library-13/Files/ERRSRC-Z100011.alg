BEGIN %                                                                         
REAL COM ; % NUMBER OF RECORDS IN ERROR FILE.                                   
  SAVE ARRAY A [0:9], B [0:50] ; %                                              
FILE OUT PRINT 16 (2,15) ; %                                                    
  INTEGER I,N,P,Y,Z  ; %                                                        
IF COM = 0 THEN COM := 999; %                                                   
BEGIN % CREATE FILE                                                             
  FILE IN CARD DISK SERIAL "ERROR""INPUT" (2,10,150); %                         
  FILE OUT CREATE DISK SERIAL [20: %                                            
    (19+COM)DIV 20 + 20]   "ALGOL""ERRORS" (2,15,30,SAVE 365) ; %               
BOOLEAN STREAM PROCEDURE CMPR(A,B) ; %                                          
BEGIN SI~A;DI~B;IF 4SC{DC THEN TALLY~1;CMPR~TALLY;END;%                         
BOOLEAN PROCEDURE CMP(A,B);ARRAY A,B[0];CMP~CMPR(A,B);%                         
PROCEDURE HV(A);ARRAY A[0];A[0]~REAL(<1414141414141414>);%                      
FILE OUT FIL DISK SERIAL [20:COM]"TEMPFIL"" "(2,10,30,SAVE 2) ; %               
  FILL B[*] WITH "SYNTAX E","RROR. PL","EASE TAK","E TO PRO","GRAMMING",        
                 " ASSISTA","NT.     ","        ","        ","        ",        
                 "        ","        ","        ","        ","        ";        
FOR I := 0 STEP 1 UNTIL COM DO WRITE(CREATE,15,B[*]) ; %                        
LOCK (CREATE) ; %                                                               
SORT (FIL,CARD,3,HV,CMP,10,6000,120000) ; %                                     
END   ; % CREATE                                                                
BEGIN % FIX FILE                                                                
FILE IN CARD DISK SERIAL "TEMPFIL"" "(2,10,30) ; %                              
FILE OUT ERRMDF DISK RANDOM "ALGOL""ERRORS" (1,15,30) ; %                       
  LABEL L,X,DEL ; %                                                             
  REAL STREAM PROCEDURE ADRS (A) ; %                                            
  BEGIN %                                                                       
    SI ~ A ; %                                                                  
    ADRS ~ SI ; %                                                               
  END ADRS ; %                                                                  
  STREAM PROCEDURE EDIT (A,V,N) ; %                                             
  VALUE V ; %                                                                   
  BEGIN %                                                                       
    LABEL X ; %                                                                 
    LOCAL L; %                                                                  
    SI ~ A ; %                                                                  
    SI ~ SI + 29 ; %                                                            
    DI ~ V ; %                                                                  
TALLY ~ 51; %                                                                   
50(IF SC = " " THEN BEGIN SI ~ SI + 1; TALLY ~ TALLY + 63 END ELSE %            
           JUMP OUT); %                                                         
    L := TALLY; %                                                               
    DS := L CHR; %                                                              
    V ~ DI ; % %                                                                
    SI ~ V ; %                                                                  
    DI ~ DI - 8 ; %                                                             
    X : %                                                                       
    IF SC = " " THEN BEGIN %                                                    
      SI ~ SI - 1 ; %                                                           
      GO X ; %                                                                  
    END ; %                                                                     
    SI ~ SI + 2 ; %                                                             
    V ~ SI ; %                                                                  
    SI ~ LOC V ; %                                                              
    DI ~ N ; %                                                                  
    DS ~ WDS ; %                                                                
  END EDIT ; %                                                                  
  STREAM PROCEDURE FINS (V) ; %                                                 
  VALUE V ; %                                                                   
  BEGIN %                                                                       
    DI ~ V ; %                                                                  
SI~V ;SI~SI-2;                                                                  
IF SC="." THEN ELSE BEGIN  DI~DI-1 ; DS~LIT "." END;                            
  END FINS ; %                                                                  
FORMAT ERR("ERROR NUMBER"I5" IS:");                                             
  STREAM PROCEDURE CLER (V) ; %                                                 
  VALUE V ; %                                                                   
  BEGIN %                                                                       
    DI ~ V ; %                                                                  
    DI ~ DI + 48 ; %                                                            
    V ~ DI ; %                                                                  
    SI ~ V ; %                                                                  
    DS ~ 8 LIT " " ; %                                                          
    DS ~ 8 WDS ; %                                                              
  END CLER ; %                                                                  
  BOOLEAN STREAM PROCEDURE ONEP (A,Z) ; %                                       
  BEGIN %                                                                       
    SI ~ A ; %                                                                  
    SI ~ SI + 3 ; %                                                             
    IF SC = "1" THEN BEGIN %                                                    
      TALLY ~ 1 ; %                                                             
      ONEP ~ TALLY ; %                                                          
      SI ~ SI - 3 ; %                                                           
      DI ~ Z ; %                                                                
      DS ~ 3 OCT ; %                                                            
    END ; %                                                                     
  END ONEP ; %                                                                  
  N ~ ADRS (B) ; %                                                              
  READ (CARD,10,A[*])[L] ; %                                                    
  IF ONEP (A,Z) THEN ; %                                                        
FOR I := 0 STEP 1 UNTIL COM DO BEGIN %                                          
      Y ~ Z ; %                                                                 
      P ~ N ; %                                                                 
      CLER (N) ; %                                                              
      DO BEGIN %                                                                
        EDIT (A,P,P) ; %                                                        
        READ (CARD,10,A[*])[L] ; %                                              
      END UNTIL ONEP (A,Z) ; %                                                  
IF FALSE THEN L: I ~ COM + 1 ; %                                                
      FINS (P) ; %                                                              
WRITE (PRINT,ERR,Y) ; %                                                         
      WRITE (ERRMDF[Y],15,B[*]) ; %                                             
WRITE(PRINT[DBL],15,B[*]) ; %                                                   
  END ; %                                                                       
CLOSE (CARD,PURGE) ; %                                                          
  LOCK (ERRMDF) ; %                                                             
DEL: %                                                                          
END ; % FIXING FILE                                                             
END .                                                                           
