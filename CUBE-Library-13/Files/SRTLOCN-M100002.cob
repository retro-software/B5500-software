000100 REMARKS  PROGRAM SRTPERM, CUBE LIBRARY NUMBER IS M100002.                
000200 REMARKS THIS VERSION DATED 06/01/67.                                     
000300 IDENTIFICATION DIVISION.                                         ZIP-SORT
000400 PROGRAM-ID. "SRTPERM".                                           ZIP-SORT
000500 ENVIRONMENT DIVISION.                                            ZIP-SORT
000600 CONFIGURATION SECTION.                                           ZIP-SORT
000700 SOURCE-COMPUTER. B-5000.                                         ZIP-SORT
000800 OBJECT-COMPUTER. B-5000 MEMORY SIZE  4000  WORDS 4 TAPES.        ZIP-SORT
000900 INPUT-OUTPUT SECTION.                                            ZIP-SORT
001000 FILE-CONTROL.                                                    ZIP-SORT
001100     SELECT INPUT-FILE ASSIGN TO TAPE.                            ZIP-SORT
001200     SELECT SORT-FILE ASSIGN TO 3 SORT-TAPES.                     ZIP-SORT
001300     SELECT OUTPUT-FILE ASSIGN TO TAPE.                           ZIP-SORT
001400 I-O-CONTROL.                                                     ZIP-SORT
001500     APPLY TECHNIQUE-A ON INPUT-FILE                              ZIP-SORT
001600     APPLY TECHNIQUE-A  ON OUTPUT-FILE.                           ZIP-SORT
001700 DATA DIVISION.                                                   ZIP-SORT
001800 FILE SECTION.                                                    ZIP-SORT
001900 FD  INPUT-FILE                                                   ZIP-SORT
002000     RECORDING MODE IS STANDARD BLOCK CONTAINS 1 RECORDS          ZIP-SORT
002100     RECORD CONTAINS 624 CHARACTERS LABEL RECORD IS STANDARD      ZIP-SORT
002200     VALUE OF ID IS "UNSORTP" DATA RECORD IS INPUT-RECORD.        ZIP-SORT
002300 01  INPUT-RECORD, SZ 624.                                        ZIP-SORT
002400 SD  SORT-FILE                                                    ZIP-SORT
002500     RECORD CONTAINS 624 CHARACTERS                               ZIP-SORT
002600     DATA RECORD IS SORT-RECORD.                                  ZIP-SORT
002700 01  SORT-RECORD.                                                 ZIP-SORT
002800     02  FILLER IS SZ 300.                                        ZIP-SORT
002900     02  SEQA PICTURE IS X(60).                                   ZIP-SORT
003000     02  SEQB PICTURE IS X(60).                                   ZIP-SORT
003100     02  SEQC PICTURE IS X(60).                                   ZIP-SORT
003200     02  SEQD PICTURE IS X(60).                                   ZIP-SORT
003300     02  SEQE PICTURE IS X(60).                                   ZIP-SORT
003400     02  FILLER IS SZ 1.                                          ZIP-SORT
003500     02  NUMB PICTURE IS X(19).                                   ZIP-SORT
003600     02  FILLER IS SZ 4.                                          ZIP-SORT
003700 FD  OUTPUT-FILE                                                  ZIP-SORT
003800     RECORDING MODE IS STANDARD BLOCK CONTAINS 1 RECORDS          ZIP-SORT
003900     RECORD CONTAINS 624 CHARACTERS LABEL RECORD IS STANDARD      ZIP-SORT
004000     VALUE OF ID IS "SORTEDP" SAVE-FACTOR IS 1                    ZIP-SORT
004100     DATA RECORD IS OUTPUT-RECORD.                                        
004200 01  OUTPUT-RECORD SZ 624.                                        ZIP-SORT
004300 PROCEDURE DIVISION.                                              ZIP-SORT
004400 SORTING SECTION.                                                 ZIP-SORT
004500 START. SORT SORT-FILE ON ASCENDING KEY                           ZIP-SORT
004600     SEQA OF SORT-RECORD                                          ZIP-SORT
004700     SEQB OF SORT-RECORD                                          ZIP-SORT
004800     SEQC OF SORT-RECORD                                          ZIP-SORT
004900     SEQD OF SORT-RECORD                                          ZIP-SORT
005000     SEQE OF SORT-RECORD                                          ZIP-SORT
005100     NUMB OF SORT-RECORD USING                                    ZIP-SORT
005200     INPUT-FILE OUTPUT PROCEDURE IS                               ZIP-SORT
005300     FINAL.                                                       ZIP-SORT
005400 AA. STOP RUN.                                                    ZIP-SORT
005500 FINAL SECTION.                                                   ZIP-SORT
005600 AC. OPEN OUTPUT OUTPUT-FILE.                                     ZIP-SORT
005700 FINISH. RETURN SORT-FILE INTO OUTPUT-RECORD                      ZIP-SORT
005800     AT END NEXT SENTENCE,ELSE                                    ZIP-SORT
005900     WRITE OUTPUT-RECORD,                                         ZIP-SORT
006000     GO TO FINISH.                                                ZIP-SORT
006100 CLOSING SECTION.                                                 ZIP-SORT
006200 AB. STOP RUN.                                                    ZIP-SORT
006300 END-OF-JOB.                                                      ZIP-SORT
006400  000000000PUNCH  00166025010000000006400000640000        0OMNI           
