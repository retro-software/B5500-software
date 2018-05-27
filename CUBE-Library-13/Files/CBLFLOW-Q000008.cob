001010 IDENTIFICATION DIVISION.                                         OR43125 
001020 PROGRAM-ID. "CBLFLOW".                                           OR43125 
001030 ENVIRONMENT DIVISION.                                            OR43125 
001040 CONFIGURATION SECTION.                                           OR43125 
001050 SOURCE-COMPUTER. B-5000.                                         OR43125 
001060 OBJECT-COMPUTER. B-5000, 7 TAPES.                                OR43125 
001070 SPECIAL-NAMES. CHANNEL 1 IS TOP.                                 OR43125 
001080 INPUT-OUTPUT SECTION.                                            OR43125 
001090 FILE-CONTROL.                                                    OR43125 
001100     SELECT COB-FILE ASSIGN TO READER.                            OR43125 
001110     SELECT PLOT-FILE ASSIGN TO TAPE.                             OR43125 
001120     SELECT FLOW-FILE ASSIGN TO PRINTER.                          OR43125 
001130 I-O-CONTROL.                                                     OR43125 
001140     APPLY TECHNIQUE-A ON PLOT-FILE.                              OR43125 
002010 DATA DIVISION.                                                   OR43125 
002020 FILE SECTION.                                                    OR43125 
002030 FD COB-FILE BLOCK CONTAINS 80 CHARACTERS LABEL RECORDS ARE       OR43125 
002040     STANDARD VALUE OF ID IS "CBLFLOW" DATA RECORDS ARE COB-REC.  OR43125 
002050 01  COB-REC.                                                     OR43125 
002060     03 COBINC  PC 9(6).                                          OR43125 
002070     03 COBSP   PC X.                                             OR43125 
002080     03 COBHD   PC X(10).                                         OR43125 
002090     03 COBRST.                                                   OR43125 
002100         05 CORST  PC X(55).                                      OR43125 
002110         05 COPRG  PC X(8).                                       OR43125 
002200 FD  PLOT-FILE BLOCK CONTAINS 90 CHARACTERS LABEL RECORDS ARE     OR43125 
002210     STANDARD VALUE OF ID IS "CBLFLOW" SAVE-FACTOR IS 05          OR43125 
002220     DATA RECORDS ARE PLOT-REC.                                   OR43125 
002230 01  PLOT-REC.                                                    OR43125 
002240     03 PLOTCHAR.                                                 OR43125 
002242         05 PLOT-1  PC X(75).                                     OR43125 
002244         05 PID     PC X.                                         OR43125 
002246         05 PLOT-2  PC X(4).                                      OR43125 
002248     03 PLOTIND  PC 9(10).                                        OR43125 
002250 FD  FLOW-FILE BLOCK CONTAINS 132 CHARACTERS LABEL RECORDS ARE    OR43125 
002260     STANDARD VALUE OF ID IS "CBLFLOW" DATA RECORDS ARE FLOW-REC. OR43125 
002270 01  FLOW-REC.                                                    OR43125 
002280     03 FLOWDATA  PC X(132).                                      OR43125 
002290 WORKING-STORAGE SECTION.                                         OR43125 
003010 77  T  PC 99.                                                    OR43125 
003020 77  P  PC 999  VA 001.                                           OR43125 
003030 77  PGNO PC 9999 VA 0001.                                        OR43125 
003040 77  L  PC 99.                                                    OR43125 
003050 77  N  PC 99.                                                    OR43125 
003060 77  LNCNTER  PC 99 VA 00.                                        OR43125 
003070 77  B  PC 9 VA 1.                                                OR43125 
003080 77  VBIND    PC 9.                                               OR43125 
003090 77  QTIND    PC 9.                                               OR43125 
003100 77  BKIND    PC 9.                                               OR43125 
003110 77  V  PC 99.                                                    OR43125 
003120 77  LCHHD  PC 99.                                                OR43125 
003130 77  INDEXHD PC 99.                                               OR43125 
003140 77  TYPRHD  PC 99.                                               OR43125 
003150 77 PRONME  PC X(8).                                              OR43125 
003160 77 SPCT    PC 9.                                                 OR43125 
003170 77 INVIND  PC 9 VA 0.                                            OR43125 
003180 77 SIND  PC 9.                                                   OR43125 
003190 77 LKIND PC 9.                                                   OR43125 
004010 01  COBTST.                                                      OR43125 
004020     03 COBPAR  PC X(4).                                          OR43125 
004030     03 COBPARB PC X(6).                                          OR43125 
004031 01  COBTSTR REDEFINES COBTST.                                    OR43125 
004032     03 COBCH  PC X OCCURS 10 TIMES.                              OR43125 
004040 01  COBSCAN.                                                     OR43125 
004050     03 CHAR   PC X OCCURS 80 TIMES.                              OR43125 
004060 01  TABHOLD.                                                     OR43125 
004070     03 TABHD  PC X OCCURS 10 TIMES.                              OR43125 
004080 01  PARATABLE.                                                   OR43125 
004090     03 PARTST  PC X(10) OCCURS 800 TIMES.                        OR43125 
004100 01  BALTABLE.                                                    OR43125 
004110     03 BALTST  PC XXX   OCCURS 800 TIMES.                        OR43125 
004120 01  PGTABLE.                                                     OR43125 
004121     03 PGTST  PC 9(4) OCCURS 800 TIMES.                          OR43125 
004122 01  PNAMER.                                                      OR43125 
004123     03 PNAME  PC X OCCURS 30 TIMES.                              OR43125 
004124 01  BALCT.                                                       OR43125 
004125     03 BLCTA  PC X VA "A".                                       OR43125 
004126     03 BLCTB  PC 99 VA 01.                                       OR43125 
004130 01  COBPAROUT.                                                   OR43125 
004140     03 COBPINC   PC 9(6).                                        OR43125 
004150     03 COBFIL-P  PC X.                                           OR43125 
004160     03 COBPAROT  PC X(30).                                       OR43125 
004170     03 COBPARSP  PC X(35).                                       OR43125 
004180     03 COBPBAL   PC XXX.                                         OR43125 
004190     03 COBPTYP   PC X.                                           OR43125 
004200     03 COBPPG   PC 9(4).                                         OR43125 
004210     03 COBPFIL  PC 9(10).                                        OR43125 
004220 01  ALPHTABLE.                                                   OR43125 
004230     03 ALA  PC X VA "A".                                         OR43125 
004240     03 ALB  PC X VA "B".                                         OR43125 
004250     03 ALC  PC X VA "C".                                         OR43125 
004260     03 ALD  PC X VA "D".                                         OR43125 
004270     03 ALE  PC X VA "E".                                         OR43125 
004280     03 ALF  PC X VA "F".                                         OR43125 
004290     03 ALG  PC X VA "G".                                         OR43125 
004300     03 ALH  PC X VA "H".                                         OR43125 
004310 01  ALSUBTBL REDEFINES ALPHTABLE.                                OR43125 
004320     03 ALSUB  PC X OCCURS 8 TIMES.                               OR43125 
004330 01  VERBER.                                                      OR43125 
004340     03 VBCH  PC X OCCURS 8 TIMES.                                OR43125 
004350 01  INDEXER.                                                     OR43125 
004360     03 INDX-1   PC 99.                                           OR43125 
004370     03 LCH-1    PC 99.                                           OR43125 
004380     03 INDX-2   PC 99.                                           OR43125 
004390     03 LCH-2    PC 99.                                           OR43125 
004400     03 TYPER    PC 99.                                           OR43125 
005010 01 PGF-1.                                                        OR43125 
005020     03 DUM-1 PC X(132) VA SPACES.                                OR43125 
005030 01 PGF-2.                                                        OR43125 
005040     03 FILLER PC X(13) VA SPACES.                                OR43125 
005050     03 PF2-1 PC XXX VA "...".                                    OR43125 
005060     03 FILLER PC X(11) VA SPACES.                                OR43125 
005070     03 PF2-2 PC X VA ".".                                        OR43125 
005080     03 FILLER PC X(104) VA SPACES.                               OR43125 
005090 01 PGF-3.                                                        OR43125 
005100     03 FILLER PC X(12) VA SPACES.                                OR43125 
005110     03 PF3-1  PC X(5) VA ".   .".                                OR43125 
005120     03 FILLER PC X(6) VA SPACES.                                 OR43125 
005130     03 PF3-2  PC X(33) VA ".................................".   OR43125 
005140     03 FILLER PC X(76) VA SPACES.                                OR43125 
005150 01 PGF-4.                                                        OR43125 
005160     03 PF4-1  PC 9(6).                                           OR43125 
005170     03 FILLER PC X(5) VA SPACES.                                 OR43125 
005180     03 PF4-2  PC XX VA ". ".                                     OR43125 
005190     03 PF4-3  PC XXX.                                            OR43125 
005200     03 PF4-4  PC X(9) VA " .-----( ".                            OR43125 
005210     03 PF4-5  PC X(30).                                          OR43125 
005220     03 PF4-6  PC XX VA " )".                                     OR43125 
005230     03 FILLER PC X(75) VA SPACES.                                OR43125 
005240 01 SPACRE.                                                       OR43125 
005250     03 FILLER PC X(27) VA SPACES.                                OR43125 
005260     03 SPC    PC X VA ".".                                       OR43125 
005270     03 FILLER PC X(104) VA SPACES.                               OR43125 
005280 01 S06A.                                                         OR43125 
005290     03 FILLER PC X(13)  VA SPACES.                               OR43125 
005300     03 S6A    PC XXX    VA "...".                                OR43125 
005310     03 FILLER PC X(11)  VA SPACES.                               OR43125 
005320     03 S6B    PC X      VA ".".                                  OR43125 
005330     03 FILLER PC X(11)  VA SPACES.                               OR43125 
005340     03 S6C    PC XXX    VA "...".                                OR43125 
005350     03 FILLER PC X(90)  VA SPACES.                               OR43125 
005360 01 S06B.                                                         OR43125 
005370     03 FILLER PC X(12)  VA SPACES.                               OR43125 
005380     03 S6D    PC X(5)   VA ".   .".                              OR43125 
005390     03 FILLER PC X(8)   VA SPACES.                               OR43125 
005400     03 S6E    PC X(5)   VA ".   .".                              OR43125 
005410     03 FILLER PC X(8)   VA SPACES.                               OR43125 
005420     03 S6F    PC X(5)   VA ".   .".                              OR43125 
005430     03 FILLER PC X(89)  VA SPACES.                               OR43125 
005440 01 S06C.                                                         OR43125 
005450     03 S6G    PC 9(6).                                           OR43125 
005460     03 FILLER PC X(5)   VA SPACES.                               OR43125 
005470     03 S6H    PC XX     VA ". ".                                 OR43125 
005480     03 S6J    PC XXX.                                            OR43125 
005490     03 S6K    PC X(10)  VA " .-----.  ".                         OR43125 
005500     03 S6L    PC XX.                                             OR43125 
005510     03 S6M    PC X(11)  VA "   .-----. ".                        OR43125 
005520     03 S6N    PC XXX.                                            OR43125 
005530     03 S6P    PC XX     VA " .".                                 OR43125 
005540     03 FILLER PC X(5)   VA SPACES.                               OR43125 
005550     03 S6R    PC X(65).                                          OR43125 
005560     03 FILLER PC X(18)  VA SPACES.                               OR43125 
005570 01 SPACRE6.                                                      OR43125 
005580     03 FILLER PC X(11)  VA SPACES.                               OR43125 
005590     03 SPC6-1 PC XXX   VA "PG ".                                 OR43125 
005600     03 SPC6-2 PC ZZZ9.                                           OR43125 
005610     03 FILLER PC X(9)   VA SPACES.                               OR43125 
005620     03 SPC6-3 PC X     VA ".".                                   OR43125 
005630     03 FILLER PC X(9)   VA SPACES.                               OR43125 
005640     03 SPC6-4 PC XXX   VA "PG ".                                 OR43125 
005650     03 SPC6-5 PC ZZZ9.                                           OR43125 
005660     03 FILLER PC X(88)  VA SPACES.                               OR43125 
006010 01 PTYP1.                                                        OR43125 
006020     03 FILLER PC X(25) VA SPACES.                                OR43125 
006030     03 PTP1   PC X(5)  VA ".   .".                               OR43125 
006040     03 FILLER PC X(102) VA SPACES.                               OR43125 
006050 01 PTYP2.                                                        OR43125 
006060     03 TP1NO  PC 9(6).                                           OR43125 
006070     03 FILLER PC X(17) VA SPACES.                                OR43125 
006080     03 PTP2.                                                     OR43125 
006090         05 PTPA PC XXX VA ".  ".                                 OR43125 
006100         05 PTPB PC XX.                                           OR43125 
006110         05 PTPC PC X(4) VA "   .".                               OR43125 
006120     03 FILLER PC X(17) VA SPACES.                                OR43125 
006130     03 PTP3   PC X(65).                                          OR43125 
006140     03 FILLER PC X(18) VA SPACES.                                OR43125 
006150 01 HDBOT.                                                        OR43125 
006160     03 FILLER PC X(25) VA SPACES.                                OR43125 
006162     03 HD1A   PC X     VA "(".                                   OR43125 
006170     03 HDNO    PC XXX.                                           OR43125 
006172     03 HD2A   PC X     VA ")".                                   OR43125 
006180     03 FILLER PC X(26) VA SPACES.                                OR43125 
006190     03 HDPG    PC X(5)  VA "PAGE ".                              OR43125 
006200     03 HDPNO   PC ZZZ9.                                          OR43125 
006210     03 FILLER   PC X(15) VA SPACES.                              OR43125 
006220     03 HDPRG   PC X(8).                                          OR43125 
006230     03 FILLER  PC X(44) VA SPACES.                               OR43125 
006250 01 PAREA.                                                        OR43125 
006260     03 P-SEQ   PC 9(6).                                          OR43125 
006270     03 P-SP    PC X.                                             OR43125 
006280     03 P-NME   PC X(30).                                         OR43125 
006290     03 P-BLK   PC X(35).                                         OR43125 
006300     03 P-BAL   PC XXX.                                           OR43125 
006310     03 P-RST   PC X(15).                                         OR43125 
006320 01 BAREA.                                                        OR43125 
006330     03 B-BLK   PC X(72).                                         OR43125 
006340     03 B-BAL   PC XXX.                                           OR43125 
006350     03 B-TY    PC X.                                             OR43125 
006360     03 B-PG    PC 9(4).                                          OR43125 
006370     03 B-RST   PC X(10).                                         OR43125 
006400 01 SAREA.                                                        OR43125 
006410     03 S-SEQ   PC 9(6).                                          OR43125 
006420     03 S-SP    PC X.                                             OR43125 
006430     03 S-STA   PC X(65).                                         OR43125 
006440     03 S-BLK   PC XXX.                                           OR43125 
006450     03 S-TY    PC X.                                             OR43125 
006460     03 S-KLB   PC X(4).                                          OR43125 
006470     03 SDX-1   PC 99.                                            OR43125 
006480     03 SCH-1   PC 99.                                            OR43125 
006490     03 SDX-2   PC 99.                                            OR43125 
006500     03 SCH-2   PC 99.                                            OR43125 
006510     03 STYPE   PC 99.                                            OR43125 
006520 01 PALT1.                                                        OR43125 
006530     03 FILLER PC X(10) VA SPACES.                                OR43125 
006540     03 PATA   PC X(3).                                           OR43125 
006550     03 FILLER PC X(7)  VA SPACES.                                OR43125 
006560     03 PATB   PC XX.                                             OR43125 
006570     03 PATC   PC X(11) VA "...........".                         OR43125 
006580     03 PATD   PC XX.                                             OR43125 
006590     03 FILLER PC X(7)  VA SPACES.                                OR43125 
006600     03 PATE   PC X(3)  VA "...".                                 OR43125 
006610     03 FILLER PC X(87) VA SPACES.                                OR43125 
006620 01 PALT2.                                                        OR43125 
006630     03 FILLER PC X(09) VA SPACES.                                OR43125 
006640     03 PA2A   PC X(5).                                           OR43125 
006650     03 FILLER PC X(6)  VA SPACES.                                OR43125 
006660     03 PA2B   PC XX.                                             OR43125 
006670     03 PA2C   PC X(11) VA ".         .".                         OR43125 
006680     03 PA2D   PC XX.                                             OR43125 
006690     03 FILLER PC X(6)  VA SPACES.                                OR43125 
006700     03 PA2E   PC X(5)  VA ".   .".                               OR43125 
006710     03 FILLER PC X(86) VA SPACES.                                OR43125 
006720 01 PALT3.                                                        OR43125 
006730     03 PA3A   PC X(6).                                           OR43125 
006740     03 FILLER PC X(02) VA SPACES.                                OR43125 
006750     03 PA3B   PC XX.                                             OR43125 
006760     03 PA3C   PC XXX.                                            OR43125 
006770     03 PA3D   PC XX.                                             OR43125 
006780     03 PA3E   PC X(5).                                           OR43125 
006790     03 PA3F   PC XX.                                             OR43125 
006800     03 PA3G   PC XX    VA ". ".                                  OR43125 
006810     03 PA3H   PC X(8).                                           OR43125 
006820     03 PA3J   PC XXX.                                            OR43125 
006830     03 PA3K   PC X(7)  VA "-----. ".                             OR43125 
006840     03 PA3L   PC XXX.                                            OR43125 
006850     03 PA3M   PC X(4)  VA " .  ".                                OR43125 
006860     03 PA3N   PC X(65).                                          OR43125 
006870     03 FILLER PC X(18) VA SPACES.                                OR43125 
007010 01 TYP2-1.                                                       OR43125 
007020     03 FILLER   PC X(22) VA SPACES.                              OR43125 
007030     03 PTR2-1   PC X(11) VA "...........".                       OR43125 
007040     03 FILLER   PC X(9)  VA SPACES.                              OR43125 
007050     03 PTR2-2   PC X     VA ".".                                 OR43125 
007060     03 FILLER   PC X(89) VA SPACES.                              OR43125 
007070 01 TYP2-2.                                                       OR43125 
007080     03 FILLER   PC X(22) VA SPACES.                              OR43125 
007090     03 PTR-3    PC X     VA ".".                                 OR43125 
007100     03 FILLER   PC X(9)  VA SPACES.                              OR43125 
007110     03 PTR-4    PC X     VA ".".                                 OR43125 
007120     03 FILLER   PC X(7)  VA SPACES.                              OR43125 
007130     03 PTR-5    PC X(5)  VA ".   .".                             OR43125 
007140     03 FILLER   PC X(87) VA SPACES.                              OR43125 
007150 01 TYP2-3.                                                       OR43125 
007160     03 PTR-6      PC 9(6).                                       OR43125 
007170     03 FILLER     PC X(16)  VA SPACES.                           OR43125 
007180     03 PTR-7      PC XX     VA ". ".                             OR43125 
007190     03 PTR-8      PC X(8).                                       OR43125 
007200     03 PTR-9      PC X(9)   VA ".-----.  ".                      OR43125 
007210     03 PTR-10     PC XX.                                         OR43125 
007220     03 PTR-11     PC X(6)   VA "   .  ".                         OR43125 
007230     03 PTR-12     PC X(65).                                      OR43125 
007240     03 FILLER     PC X(18)  VA SPACES.                           OR43125 
007250 01 SPACRE2.                                                      OR43125 
007260     03 FILLER     PC X(42)  VA SPACES.                           OR43125 
007270     03 PTR-13     PC X      VA ".".                              OR43125 
007280     03 FILLER     PC X(89)  VA SPACES.                           OR43125 
007290 01 TYP2-4.                                                       OR43125 
007300     03 FILLER  PC X(27)  VA SPACES.                              OR43125 
007310     03 PTR-14  PC X(6)   VA "------".                            OR43125 
007320     03 PTR-15  PC X.                                             OR43125 
007330     03 PTR-16  PC X(8)   VA "--------".                          OR43125 
007340     03 FILLER  PC X(90)  VA SPACES.                              OR43125 
007350 01 T357A.                                                        OR43125 
007360     03 FILLER  PC X(20) VA SPACES.                               OR43125 
007370     03 A357    PC X(15).                                         OR43125 
007380     03 FILLER  PC X(7)  VA SPACES.                               OR43125 
007390     03 B357    PC X     VA ".".                                  OR43125 
007400     03 FILLER  PC X(11) VA SPACES.                               OR43125 
007410     03 C357    PC XXX   VA "...".                                OR43125 
007420     03 FILLER  PC X(75) VA SPACES.                               OR43125 
007430 01 T357B.                                                        OR43125 
007440     03 FILLER  PC X(20) VA SPACES.                               OR43125 
007450     03 D357    PC X(14).                                         OR43125 
007460     03 FILLER  PC X(6)  VA SPACES.                               OR43125 
007470     03 E357    PC X(5)  VA ".   .".                              OR43125 
007480     03 FILLER  PC X(8)  VA SPACES.                               OR43125 
007490     03 F357    PC X(5)  VA ".   .".                              OR43125 
007500     03 FILLER  PC X(84) VA SPACES.                               OR43125 
007510 01 T357C.                                                        OR43125 
007520     03 G357    PC 9(6).                                          OR43125 
007530     03 FILLER  PC X(16) VA SPACES.                               OR43125 
007540     03 H357    PC XX    VA ". ".                                 OR43125 
007550     03 J357    PC X(8).                                          OR43125 
007560     03 K357    PC X(22) VA ".-----.  GO   .-----. ".             OR43125 
007570     03 L357    PC XXX.                                           OR43125 
007580     03 M357    PC XX    VA " .".                                 OR43125 
007590     03 FILLER  PC X(73) VA SPACES.                               OR43125 
007600 01 TENA.                                                         OR43125 
007610     03 FILLER PC X(20)  VA SPACES.                               OR43125 
007620     03 TA01   PC X(15).                                          OR43125 
007630     03 FILLER PC X(97)  VA SPACES.                               OR43125 
007640 01 TENB.                                                         OR43125 
007650     03 FILLER PC X(20)  VA SPACES.                               OR43125 
007660     03 TA02   PC X(14).                                          OR43125 
007670     03 FILLER PC X(98)  VA SPACES.                               OR43125 
007680 01 TENC.                                                         OR43125 
007690     03 TA03   PC 9(6).                                           OR43125 
007700     03 FILLER PC X(16)  VA SPACES.                               OR43125 
007710     03 TA04   PC XX     VA ". ".                                 OR43125 
007720     03 TA05   PC X(8).                                           OR43125 
007730     03 TA06   PC X      VA ".".                                  OR43125 
007740     03 FILLER PC X(16) VA SPACES.                                OR43125 
007750     03 TA07   PC X(65).                                          OR43125 
007760     03 FILLER PC X(18) VA SPACES.                                OR43125 
008010 01 VEBTABLE.                                                     OR43125 
008020     03 VB01  PC X(8) VA "IF      ".                              OR43125 
008030     03 VB02  PC X(8) VA "GO      ".                              OR43125 
008040     03 VB03  PC X(8).                                            OR43125 
008050     03 VB04  PC X(8).                                            OR43125 
008060     03 VB05  PC X(8) VA "MOVE    ".                              OR43125 
008070     03 VB06  PC X(8) VA "ADD     ".                              OR43125 
008080     03 VB07  PC X(8) VA "SUBTRACT".                              OR43125 
008090     03 VB08  PC X(8) VA "MULTYPLY".                              OR43125 
008100     03 VB09  PC X(8) VA "DIVIDE  ".                              OR43125 
008110     03 VB10  PC X(8) VA "STOP    ".                              OR43125 
008120     03 VB11  PC X(8) VA "SORT    ".                              OR43125 
008130     03 VB12  PC X(8) VA "ALTER   ".                              OR43125 
008140     03 VB13  PC X(8) VA "COMPUTE ".                              OR43125 
008150     03 VB14  PC X(8) VA "EXAMINE ".                              OR43125 
008160     03 VB15  PC X(8) VA "EXIT    ".                              OR43125 
008170     03 VB16  PC X(8) VA "INCLUDE ".                              OR43125 
008180     03 VB17  PC X(8).                                            OR43125 
008190     03 VB18  PC X(8).                                            OR43125 
008200     03 VB19  PC X(8).                                            OR43125 
008210     03 VB20  PC X(8) VA "OPEN    ".                              OR43125 
008220     03 VB21  PC X(8) VA "CLOSE   ".                              OR43125 
008230     03 VB22  PC X(8) VA "READ    ".                              OR43125 
008240     03 VB23  PC X(8) VA "WRITE   ".                              OR43125 
008250     03 VB24  PC X(8) VA "RELEASE ".                              OR43125 
008260     03 VB25  PC X(8) VA "RETURN  ".                              OR43125 
008270     03 VB26  PC X(8) VA "RERUN   ".                              OR43125 
008280     03 VB27  PC X(8) VA "SEEK    ".                              OR43125 
008290     03 VB28  PC X(8) VA "DISPLAY ".                              OR43125 
008300     03 VB29  PC X(8).                                            OR43125 
008310     03 VB30  PC X(8) VA "ACCEPT  ".                              OR43125 
008320     03 VB31  PC X(8) VA "PERFORM ".                              OR43125 
008330 01 VEBSUBTAB REDEFINES VEBTABLE.                                 OR43125 
008340     03 VTAB        PC X(8) OCCURS 31 TIMES.                      OR43125 
009010 01 TYP3.                                                         OR43125 
009020     03 FILLER  PC X(27)  VA SPACES.                              OR43125 
009030     03 TP3-1   PC X      VA ".".                                 OR43125 
009040     03 FILLER  PC X(13)  VA SPACES.                              OR43125 
009050     03 TP3-2   PC X      VA ".".                                 OR43125 
009060     03 FILLER  PC X(11)  VA SPACES.                              OR43125 
009070     03 TP3-3   PC XXX    VA "...".                               OR43125 
009080     03 FILLER  PC X(76)  VA SPACES.                              OR43125 
009090 01 TYP3-1.                                                       OR43125 
009100     03 FILLER  PC X(25)  VA SPACES.                              OR43125 
009110     03 TP3-4   PC X(5)   VA ".   .".                             OR43125 
009120     03 FILLER  PC X(9)   VA SPACES.                              OR43125 
009130     03 TP3-5   PC X(5)   VA ".   .".                             OR43125 
009140     03 FILLER  PC X(8) VA SPACES.                                OR43125 
009150     03 TP3-6  PC X(5)    VA ".   .".                             OR43125 
009160     03 FILLER PC X(75)   VA SPACES.                              OR43125 
009170 01 TYP3-2.                                                       OR43125 
009180     03 TP3-7  PC 9(6).                                           OR43125 
009190     03 FILLER PC X(17)  VA SPACES.                               OR43125 
009200     03 TP3-8  PC XXX    VA ".  ".                                OR43125 
009210     03 TP3-9  PC XX.                                             OR43125 
009220     03 TP3-10 PC X(12)  VA "   .-----.  ".                       OR43125 
009230     03 TP3-11 PC XX.                                             OR43125 
009240     03 TP3-12 PC X(11)  VA "   .-----. ".                        OR43125 
009250     03 TP3-13 PC XXX.                                            OR43125 
009260     03 TP3-14 PC XX     VA " .".                                 OR43125 
009270     03 FILLER PC X(74)  VA SPACES.                               OR43125 
009290 01 SPACRE3.                                                      OR43125 
009300     03 FILLER PC X(27)  VA SPACES.                               OR43125 
009310     03 SPC3-1 PC X      VA ".".                                  OR43125 
009320     03 FILLER PC X(23)  VA SPACES.                               OR43125 
009330     03 SPC3-2 PC XXX    VA "PG ".                                OR43125 
009340     03 SPC3-3 PC ZZZ9.                                           OR43125 
009350     03 FILLER PC X(74)  VA SPACES.                               OR43125 
009360 01 SPACRE4.                                                      OR43125 
009370     03 FILLER PC X(27)  VA SPACES.                               OR43125 
009380     03 SPC4-1 PC X      VA ".".                                  OR43125 
009390     03 FILLER PC X(21)  VA SPACES.                               OR43125 
009400     03 SPC4-2 PC X(65).                                          OR43125 
009410     03 FILLER PC X(18)  VA SPACES.                               OR43125 
009450 01 TYP4.                                                         OR43125 
009460     03 FILLER  PC X(27) VA SPACES.                               OR43125 
009470     03 TP4-1   PC X     VA ".".                                  OR43125 
009480     03 FILLER  PC X(11) VA SPACES.                               OR43125 
009490     03 TP4-2   PC XXX   VA "...".                                OR43125 
009500     03 FILLER  PC X(90) VA SPACES.                               OR43125 
009510 01 TYP4-1.                                                       OR43125 
009520     03 FILLER  PC X(25) VA SPACES.                               OR43125 
009530     03 TP4-3   PC X(5)  VA ".   .".                              OR43125 
009540     03 FILLER  PC X(8)  VA SPACES.                               OR43125 
009550     03 TP4-4   PC X(5)  VA ".   .".                              OR43125 
009560     03 FILLER  PC X(89) VA SPACES.                               OR43125 
009570 01 SPACRE5.                                                      OR43125 
009580     03 FILLER  PC X(27) VA SPACES.                               OR43125 
009590     03 TP4-15  PC X     VA ".".                                  OR43125 
009600     03 FILLER  PC X(9)  VA SPACES.                               OR43125 
009610     03 TP4-16  PC XXX   VA "PG ".                                OR43125 
009620     03 TP4-17  PC ZZZ9.                                          OR43125 
009630     03 FILLER  PC X(88) VA SPACES.                               OR43125 
009570 01 TYP4-2.                                                       OR43125 
009580     03 TP4-5   PC 9(6).                                          OR43125 
009590     03 FILLER  PC X(17) VA SPACES.                               OR43125 
009600     03 TP4-6   PC X(3)  VA ".  ".                                OR43125 
009610     03 TP4-7   PC XX.                                            OR43125 
009620     03 TP4-8   PC X(11) VA "   .-----. ".                        OR43125 
009630     03 TP4-9   PC XXX.                                           OR43125 
009640     03 TP4-10  PC XX    VA " .".                                 OR43125 
009650     03 FILLER  PC X(5)  VA SPACES.                               OR43125 
009660     03 TP4-11  PC X(65).                                         OR43125 
009670     03 FILLER  PC X(18) VA SPACES.                               OR43125 
009671 PROCEDURE DIVISION.                                                      
009672 DICTIONARY-NOTE.                                                         
009673     NOTE BEGIN DICTIONARY ENTRY                                          
009674      PROGRAM-ID:   9UTL46R/CBLFLOW                                       
009675      SYNOPSIS:     GIVEN A SOURCE DECK LESS THE CONTROL CARDS            
009676                    THIS PROGRAM WILL PRINT A FLOWCHART OF THAT           
009677                    PROGRAM ON THE LINE PRINTER                           
009678      AUTHOR:       SANDY DEVASSIE, BURROUGHS SAN ANTONIO TEXAS           
009679      DATE-WRITTEN: OCTOBER 13, 1967                                      
009680      PUBLISHER:    SOFTWARE SERVICES-CHASE BRASS AND COPPER              
009681      DATE-                                                               
009682         PUBLISHED: FEBRUARY 1, 1969                                      
009683      LANGUAGE:     B-5500 COBOL                                          
009684      INPUT:        FILE-ID: COB-FILE                                     
009685                    A) ONLY THE PROCEDURE DIVISION OF THE SOURCE          
009686                    DECK NEED BE USED; THE SCAN OF THE SOURCE DECK        
009687                    FOR THE GENERATION OF THE FLOWCHART BEGINS WITH       
009688                    THE PROCEDURE DIVISION SO THE OTHER DIVISIONS         
009689                    MAY BE DELETED: NO EDITING IS NEEDED FOR THE          
009690                    SOURCE DECK;                                          
009691                    B) THE PROGRAM-ID IS PICKED OFF OF THE ID FIELD       
009692                    (COLUMN 73-80) OF THE PROCEDURE DIVISION CARD         
009693                    AND PUT AT THE TOP OF EACH PAGE OF THE FLOWCHART      
009694                    C) CONTROL CARD SET-UP FOR THE B-5500:                
009695                         CC EXECUTE 9UTL46R/CBLFLOW                       
009696                         CC DATA CBLFLOW                                  
009697                         SOURCE DECK                                      
009698                         CC EOF                                           
009699                    D) TO EXECUTE, YOU NEED:                              
009700                         1  PRINTER                                       
009701                         2  CARD-READER                                   
009702                         3  SCRATCH  TAPE                                 
009703      OUTPUT:       FILE-ID: CBLFLOW                                      
009704                    A) THE FLOWCHART IS UNI-DIRECTIONAL AND SENT OUT      
009705                    TO THE LINE-PRINTER;                                  
009706                    B) THE CHART IS PRINTED AT A RATE OF 6 LINES PER      
009707                    INCH;  THUS CAN BE ALTERED BY CHANGING CARD           
009708                    NUMBER 010400:  IF LNCNTER = 6 GO TO P9               
009709                    THE "6" IS THE NUMBER OF LINES PER INCH;              
010010                                                                          
010011                                                                          
010019         END DICTIONARY ENTRY.                                    OR43125 
010020 BEGIN.                                                           OR43125 
010030     OPEN INPUT COB-FILE.                                         OR43125 
010040     OPEN OUTPUT PLOT-FILE.                                       OR43125 
010100 P1. READ COB-FILE AT END GO TO LISTUM.                           OR43125 
010110     IF COBHD ! "PROCEDURE " GO TO P1.                            OR43125 
010112     MOVE COPRG TO HDPRG.                                         OR43125 
010120 P2. READ COB-FILE AT END GO TO LISTUM.                           OR43125 
010130     MOVE COBHD TO COBTST.                                        OR43125 
010140     IF COBPAR EQUALS SPACES MOVE COB-REC TO COBSCAN GO TO P11.   OR43125 
010150     MOVE SPACES TO TABHOLD.                                      OR43125 
010160     MOVE ZEROS TO T.                                             OR43125 
010170 P3. ADD 1 TO T.                                                  OR43125 
010180     IF T EXCEEDS 10 GO TO P4.                                    OR43125 
010190     IF COBCH (T) EQUALS SPACES GO TO P4.                         OR43125 
010200     MOVE COBCH (T) TO TABHD (T) GO TO P3.                        OR43125 
010210 P4. MOVE TABHOLD TO PARTST (P).                                  OR43125 
010220     MOVE BALCT TO BALTST (P).                                    OR43125 
010230     MOVE PGNO TO PGTST (P).                                      OR43125 
010240     ADD 1 TO P.                                                  OR43125 
010250     MOVE COB-REC TO COBSCAN.                                     OR43125 
010260     MOVE 7 TO L MOVE 01 TO N MOVE SPACES TO PNAMER.              OR43125 
010270 P5. ADD 1 TO L.                                                  OR43125 
010280     IF CHAR (L) EQUALS SPACES GO TO P6.                          OR43125 
010290     MOVE CHAR (L) TO PNAME (N).                                  OR43125 
010300     ADD 1 TO N GO TO P5.                                         OR43125 
010310 P6. MOVE COBINC TO COBPINC.                                      OR43125 
010320     MOVE PNAMER TO COBPAROT.                                     OR43125 
010330     MOVE BALCT TO COBPBAL.                                       OR43125 
010332     MOVE "P" TO COBPTYP.                                         OR43125 
010334     MOVE PGNO TO COBPPG.                                         OR43125 
010340     WRITE PLOT-REC FROM COBPAROUT.                               OR43125 
010350     IF BLCTB EQUALS 99 GO TO P7.                                 OR43125 
010360     ADD 1 TO BLCTB GO TO P8.                                     OR43125 
010370 P7. ADD 1 TO B.                                                  OR43125 
010380     MOVE ALSUB (B) TO BLCTA MOVE 1 TO BLCTB.                     OR43125 
010390 P8. ADD 1 TO LNCNTER.                                            OR43125 
010400     IF LNCNTER EQUALS 06 GO TO P9.                               OR43125 
010410     GO TO P10.                                                   OR43125 
010420 P9. MOVE SPACES TO COBPAROUT.                                    OR43125 
010430     MOVE PGNO TO COBPPG.                                         OR43125 
010440     MOVE BALCT TO COBPBAL MOVE "B" TO COBPTYP.                   OR43125 
010450     ADD 1 TO PGNO MOVE ZEROS TO LNCNTER.                         OR43125 
010460     WRITE PLOT-REC FROM COBPAROUT.                               OR43125 
010461     IF BLCTB EQUALS 99 ADD 1 TO B MOVE ALSUB (B) TO BLCTA                
010462     MOVE 0 TO BLCTB.                                                     
010464     ADD 1 TO BLCTB.                                                      
010470 P10. IF SIND EQUALS 1 MOVE ZERO TO SIND GO TO P2.                OR43125 
010472     ADD 1 TO L.                                                  OR43125 
010480     IF L EXCEEDS 72 GO TO P2.                                    OR43125 
010490     IF CHAR (L) EQUALS SPACES GO TO P10.                         OR43125 
010500     SUBTRACT 1 FROM L GO TO P12.                                 OR43125 
010480     IF L EXCEEDS 72 GO TO P2.                                    OR43125 
010490     IF CHAR (L) EQUALS SPACES GO TO P10.                         OR43125 
010500     SUBTRACT 1 FROM L GO TO P12.                                 OR43125 
011010 P11. MOVE 11 TO L.                                               OR43125 
011020 P12. ADD 1 TO L.                                                 OR43125 
011050     IF L EXCEEDS 72 GO TO P20.                                   OR43125 
011060     IF CHAR (L) EQUALS SPACES MOVE ZERO TO VBIND GO TO P12.      OR43125 
011070     IF CHAR (L) EQUALS QUOTE GO TO P13.                          OR43125 
011080     IF QTIND EQUALS 1 GO TO P12.                                 OR43125 
011090     IF CHAR (L) EQUALS "(" OR ")" GO TO P14.                     OR43125 
011100     IF BKIND EQUALS 1 GO TO P12.                                 OR43125 
011110     IF VBIND EQUALS ZERO MOVE ZEROS TO V GO TO PXTRA.            OR43125 
011120     GO TO P12.                                                   OR43125 
011130 P13. IF QTIND EQUALS 1 MOVE ZERO TO QTIND GO TO P12.             OR43125 
011140     MOVE 1 TO QTIND GO TO P12.                                   OR43125 
011150 P14. IF BKIND EQUALS 1 MOVE ZERO TO BKIND GO TO P12.             OR43125 
011152     IF CHAR(L) EQUALS ")" MOVE ZEROS TO BKIND GO TO P12.         OR43125 
011160     MOVE 1 TO BKIND GO TO P12.                                   OR43125 
011165 PXTRA.                                                           OR43125 
011170     IF CHAR (L) EQUALS "B" OR "F" OR "H" OR "J" OR "K" OR "L"    OR43125 
011180     OR "Q"        OR "U" OR "V" OR "X" OR "Y" OR "Z"             OR43125 
011190     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
011200     MOVE SPACES TO VERBER.                                       OR43125 
011210     MOVE 00 TO V.                                                OR43125 
011220 P15. ADD 1 TO V.                                                 OR43125 
011230     IF CHAR (L) EQUALS SPACES GO TO P16.                         OR43 25 
011240     IF V EXCEEDS 8 MOVE 1 TO VBIND GO TO P12.                            
011250     MOVE CHAR (L) TO VBCH (V).                                   OR43125 
011255     IF L EXCEEDS 72 GO TO P20.                                           
011260     ADD 1 TO L GO TO P15.                                        OR43125 
011270 P16. IF V EQUALS 2 GO TO P12.                                    OR43125 
011280     IF VERBER EQUALS "IF" GO TO IF2.                             OR43125 
011290     IF VERBER EQUALS "GO" GO TO GO2.                             OR43125 
011300     MOVE 10 TO TYPRHD.                                           OR43125 
011310     IF VERBER EQUALS "MOVE" MOVE 05 TO INDEXHD GO TO K1.         OR43125 
011320     IF VERBER EQUALS "ADD"  MOVE 06 TO INDEXHD GO TO K1.         OR43125 
012010     IF VERBER EQUALS "SUBTRACT" MOVE 07 TO INDEXHD GO TO K1.     OR43125 
012020     IF VERBER EQUALS "MULTIPLY" MOVE 08 TO INDEXHD GO TO K1.     OR43125 
012030     IF VERBER EQUALS "DIVIDE"   MOVE 09 TO INDEXHD GO TO K1.     OR43125 
012040     IF VERBER EQUALS "STOP"     MOVE 10 TO INDEXHD GO TO K1.     OR43125 
012050     IF VERBER EQUALS "SORT"     MOVE 11 TO INDEXHD GO TO K1.     OR43125 
012060     IF VERBER EQUALS "ALTER" MOVE 12 TO INDEXHD GO TO K2.        OR43125 
012062     IF VERBER EQUALS "PROCEED" GO TO K3.                         OR43125 
012070     IF VERBER EQUALS "COMPUTE"  MOVE 13 TO INDEXHD GO TO K1.     OR43125 
012080     IF VERBER EQUALS "EXAMINE"  MOVE 14 TO INDEXHD GO TO K1.     OR43125 
012090     IF VERBER EQUALS "EXIT"     MOVE 15 TO INDEXHD GO TO K1.     OR43125 
012100     IF VERBER EQUALS "INCLUDE"  MOVE 16 TO INDEXHD GO TO K1.     OR43125 
012110     MOVE 11 TO TYPRHD.                                           OR43125 
012120     IF VERBER EQUALS "OPEN"     MOVE 20 TO INDEXHD GO TO K1.     OR43125 
012130     IF VERBER EQUALS "CLOSE"    MOVE 21 TO INDEXHD GO TO K1.     OR43125 
012140     IF VERBER EQUALS "READ"     MOVE 22 TO INDEXHD GO TO K1.     OR43125 
012150     IF VERBER EQUALS "WRITE"    MOVE 23 TO INDEXHD GO TO K1.     OR43125 
012160     IF VERBER EQUALS "RELEASE"  MOVE 24 TO INDEXHD GO TO K1.     OR43125 
012170     IF VERBER EQUALS "RETURN"   MOVE 25 TO INDEXHD GO TO K1.     OR43125 
012180     IF VERBER EQUALS "RERUN"    MOVE 26 TO INDEXHD GO TO K1.     OR43125 
012190     IF VERBER EQUALS "SEEK"     MOVE 27 TO INDEXHD GO TO K1.     OR43125 
012200     IF VERBER EQUALS "DISPLAY"  MOVE 28 TO INDEXHD GO TO K1.     OR43125 
012210     MOVE 12 TO TYPRHD.                                           OR43125 
012220     IF VERBER EQUALS "ACCEPT"   MOVE 30 TO INDEXHD GO TO K1.     OR43125 
012230     MOVE 13 TO TYPRHD.                                           OR43125 
012220     IF VERBER EQUALS "PERFORM" MOVE 31 TO INDEXHD GO TO K2.      OR43125 
012220     IF VERBER EQUALS "THRU" GO TO K4.                            OR43125 
012250     MOVE ZEROS TO VBIND GO TO P12.                               OR43125 
012260 K1. IF INDX-1 EQUALS ZERO MOVE INDEXHD TO INDX-1                 OR43125 
012270     MOVE TYPRHD TO TYPER MOVE ZEROS TO VBIND.                    OR43125 
012280     GO TO P12.                                                   OR43125 
012290 K2. MOVE ZEROS TO INDEXER.                                       OR43125 
012300     MOVE INDEXHD TO INDX-1.                                      OR43125 
012310     MOVE TYPRHD TO TYPER.                                        OR43125 
012320     MOVE L TO LCH-2 ADD 1 TO LCH-2.                              OR43125 
012330     MOVE ZEROS TO VBIND GO TO P12.                               OR43125 
012340 K3. MOVE L TO LCH-1 ADD 4 TO LCH-1.                              OR43125 
012350     MOVE ZEROS TO VBIND GO TO P12.                               OR43125 
012360 K4. MOVE L TO LCH-1 ADD 1 TO LCH-1.                              OR43125 
012370     MOVE ZEROS TO VBIND GO TO P12.                               OR43125 
011500 P20. MOVE COB-REC TO PLOTCHAR.                                   OR43125 
011510     MOVE "S" TO PID.                                             OR43125 
011520     MOVE INDEXER TO PLOTIND.                                     OR43125 
011530     WRITE PLOT-REC.                                              OR43125 
011540     MOVE ZEROS TO INDEXER.                                       OR43125 
011550     MOVE 1 TO SIND GO TO P8.                                     OR43125 
011560 LISTUM.                                                          OR43125 
011570     CLOSE COB-FILE.                                              OR43125 
011580     CLOSE PLOT-FILE.                                             OR43125 
011590     OPEN OUTPUT FLOW-FILE.                                       OR43125 
011600     OPEN INPUT PLOT-FILE.                                        OR43125 
011610     GO TO T1.                                                    OR43125 
014010 IF2.                                                             OR43125 
014020     IF INDX-1 EQUALS ZERO MOVE 01 TO INDX-1 MOVE 01 TO TYPER     OR43125 
014030     MOVE ZERO TO VBIND GO TO P12.                                OR43125 
014040     IF INDX-1 EQUALS 01 MOVE ZERO TO VBIND GO TO P12.            OR43125 
014050     IF INDX-1 EXCEEDS 04 MOVE 01 TO INDX-2 MOVE 02 TO TYPER      OR43125 
014060     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
014070     IF INDX-1 EQUALS 02 GO TO IF3.                               OR43125 
014080     IF INDX-2 EQUALS 00 GO TO P12.                               OR43125 
014090     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
014100 IF3. MOVE 02 TO INDX-2 MOVE 01 TO INDX-1.                        OR43125 
014110     MOVE LCH-1 TO LCH-2.                                         OR43125 
014120     MOVE ZEROS TO LCH-1.                                         OR43125 
014130     MOVE 03 TO TYPER.                                            OR43125 
014140     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
015010 GO2.                                                             OR43125 
015020     MOVE L TO LCHHD ADD 4 TO LCHHD.                              OR43125 
015022     IF TYPER EQUALS 03 OR 05 OR 07 GO TO G05.                    OR43125 
015030     IF INDX-1 EQUALS ZERO MOVE 02 TO INDX-1 MOVE 04 TO TYPER     OR43125 
015035     MOVE LCHHD TO LCH-1                                          OR43125 
015040     MOVE ZEROS TO VBIND GO TO P12.                               OR43125 
015050     IF INDX-1 EQUALS 01 MOVE 02 TO INDX-2 MOVE LCHHD TO LCH-2    OR43125 
015060     MOVE 03 TO TYPER MOVE 1 TO VBIND GO TO P12.                  OR43125 
015070     IF INDX-1 EXCEEDS 04 GO TO GO3.                              OR43125 
015080     IF INDX-1 EQUALS 02 MOVE 06 TO TYPER GO TO GO4.              OR43125 
015090     IF INDX-2 EQUALS ZEROS GO TO P12.                            OR43125 
015100     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
015110 GO3. IF INDX-1 EQUALS 22 OR 23 MOVE 07 TO TYPER ELSE             OR43125 
015120     MOVE 05 TO TYPER.                                            OR43125 
015122     MOVE LCHHD TO LCH-2 MOVE 02 TO INDX-2.                       OR43125 
015124     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
015125 GO4. MOVE LCH-1 TO LCH-2.                                        OR43125 
015130     MOVE LCHHD TO LCH-1 MOVE 02 TO INDX-2                        OR43125 
015140     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
015150 G05. MOVE 02 TO INDX-1.                                          OR43125 
015160     MOVE LCHHD TO LCH-1.                                         OR43125 
015170     MOVE 06 TO TYPER.                                            OR43125 
015180     MOVE 1 TO VBIND GO TO P12.                                   OR43125 
025010 T1. MOVE "A00" TO HDNO MOVE 0001 TO HDPNO.                       OR43125 
025020     WRITE FLOW-REC FROM PGF-1 AFTER ADVANCING TOP.               OR43125 
025030     WRITE FLOW-REC FROM HDBOT BEFORE ADVANCING 1 LINES.          OR43125 
025040 T2. READ PLOT-FILE AT END GO TO FINISH.                          OR43125 
025050     IF PID EQUALS "P" GO TO PID1.                                OR43125 
025060     IF PID EQUALS "B" GO TO BID1.                                OR43125 
025070     IF PID EQUALS "S" GO TO SID1.                                OR43125 
025080     MOVE ZERO TO SPCT.                                           OR43125 
025090 T3. WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
025100     ADD 1 TO SPCT.                                               OR43125 
025110     IF SPCT EQUALS 8 GO TO T2.                                   OR43125 
025120     GO TO T3.                                                    OR43125 
026010 PID1.                                                            OR43125 
026020     MOVE PLOT-REC TO PAREA.                                      OR43125 
026030     MOVE P-SEQ TO PF4-1.                                         OR43125 
026040     MOVE P-BAL TO PF4-3.                                         OR43125 
026050     MOVE P-NME TO PF4-5.                                         OR43125 
026060     WRITE FLOW-REC FROM PGF-2 BEFORE ADVANCING 1 LINES.          OR43125 
026070     WRITE FLOW-REC FROM PGF-3 BEFORE ADVANCING 1 LINES.          OR43125 
026080     WRITE FLOW-REC FROM PGF-4 BEFORE ADVANCING 1 LINES.          OR43125 
026090     WRITE FLOW-REC FROM PGF-3 BEFORE ADVANCING 1 LINES.          OR43125 
026100     WRITE FLOW-REC FROM PGF-2 BEFORE ADVANCING 1 LINES.          OR43125 
026110     MOVE ZERO TO SPCT.                                           OR43125 
026120 PDI2. WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.       OR43125 
026130     ADD 1 TO SPCT IF SPCT EQUALS 3 GO TO T2.                     OR43125 
026140     GO TO PDI2.                                                  OR43125 
026150 BID1. MOVE PLOT-REC TO BAREA.                                    OR43125 
026160     MOVE B-BAL TO HDNO.                                          OR43125 
026170     MOVE B-PG TO HDPNO.                                          OR43125 
026172     WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
026180     WRITE FLOW-REC FROM HDBOT BEFORE ADVANCING TOP.              OR43125 
026190     ADD 1 TO B-PG MOVE B-PG TO HDPNO.                            OR43125 
026200     WRITE FLOW-REC FROM HDBOT BEFORE ADVANCING 1 LINES.          OR43125 
026202     WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
026210     GO TO T2.                                                    OR43125 
026300 SID1. MOVE PLOT-REC TO SAREA.                                    OR43125 
026310     IF STYPE EQUALS 01 GO TO SID2.                               OR43125 
02632      IF STYPE EQUALS 02 GO TO SID4.                               OR43125 
026330     IF STYPE EQUALS 03 GO TO SID5.                               OR43125 
026340     IF STYPE EQUALS 04 GO TO SID8.                               OR43125 
026345     IF STYPE EQUALS 05 GO TO S5D1.                               OR43125 
026350     IF STYPE EQUALS 06 GO TO S6DA.                               OR43125 
026355     IF STYPE  EQUALS 07 GO TO S7D1.                              OR43125 
026356     IF SDX-1 EQUALS 12 GO TO ALT1.                               OR43125 
026357     IF STYPE EQUALS 10 GO TO S10A.                               OR43125 
026360     IF STYPE EQUALS 11 GO TO S11A.                               OR43125 
026365     IF STYPE EQUALS 12 GO TO S12A.                               OR43125 
026375     IF STYPE EQUALS 13 GO TO PFM1.                               OR43125 
026385     GO TO UNKA.                                                  OR43125 
026390 SID2.                                                            OR43125 
026400     WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
026410     WRITE FLOW-REC FROM PTYP1 BEFORE ADVANCING 1 LINES.          OR43125 
026420     MOVE S-SEQ TO TP1NO.                                         OR43125 
026430     MOVE "IF" TO PTPB.                                           OR43125 
026440     MOVE S-STA TO PTP3.                                          OR43125 
027250 SA. MOVE S-STA TO SPC4-2.                                        OR43125 
027260     WRITE FLOW-REC FROM TYP3-2 BEFORE ADVANCING 1 LINES.         OR43125 
027270     WRITE FLOW-REC FROM TYP3-1 BEFORE ADVANCING 1 LINES.         OR43125 
027280     WRITE FLOW-REC FROM TYP3   BEFORE ADVANCING 1 LINES.         OR43125 
030240     MOVE SPACES TO S6N.                                          OR43125 
030250     MOVE ZEROS TO SPC6-5.                                        OR43125 
030255     MOVE SCH-1 TO L.                                             OR43125 
030260     MOVE 1 TO LKIND GO TO L1.                                    OR43125 
030270 S8. MOVE ZEROS TO INVIND MOVE SPACES TO S6J.                     OR43125 
030280     MOVE ZEROS TO SPC6-2 GO TO S6.                               OR43125 
031010 UNKA.                                                            OR43125 
031020     WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
031030     WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
031040     MOVE S-SEQ TO TA03.                                          OR43125 
031050     MOVE S-STA TO TA07.                                          OR43125 
031060     MOVE SPACES TO TA04 MOVE SPACES TO TA06.                     OR43125 
031062     MOVE "   .    " TO TA05.                                             
031070     WRITE FLOW-REC FROM TENC BEFORE ADVANCING 1 LINES.           OR43125 
031080     MOVE ". " TO TA04.                                           OR43125 
031090     MOVE "." TO TA06.                                            OR43125 
031100     MOVE ZEROS TO SPCT.                                          OR43125 
031110 UNKB. WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.       OR43125 
031120     ADD 1 TO SPCT.                                               OR43125 
031130     IF SPCT EQUALS 5 GO TO T2.                                   OR43125 
031140     GO TO UNKB.                                                  OR43125 
050020     MOVE SCH-2 TO L.                                             OR43125 
030220     IF LKIND EQUALS 1 GO TO S8.                                  OR43125 
030230     MOVE ZEROS TO INVIND.                                        OR43125 
026450     WRITE FLOW-REC FROM PTYP2 BEFORE ADVANCING 1 LINES.          OR43125 
026460     WRITE FLOW-REC FROM PTYP1 BEFORE ADVANCING 1 LINES.          OR43125 
026470     MOVE ZERO TO SPCT.                                           OR43125 
026480 SID3. WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.       OR43125 
026490     ADD 1 TO SPCT.                                               OR43125 
026500     IF SPCT EQUALS 4 GO TO T2.                                   OR43125 
026510     GO TO SID3.                                                  OR43125 
027010 SID4. WRITE FLOW-REC FROM TYP2-1 BEFORE ADVANCING 1 LINES.       OR43125 
027020     WRITE FLOW-REC FROM TYP2-2 BEFORE ADVANCING 1 LINES.         OR43125 
027030     MOVE S-SEQ TO PTR-6.                                         OR43125 
027040     MOVE "IF" TO PTR-10.                                         OR43125 
027050     MOVE S-STA TO PTR-12.                                        OR43125 
027060     MOVE SDX-1 TO V.                                             OR43125 
027062     IF V EQUALS ZERO MOVE SPACES TO PTR-8 GO TO SD4.             OR43125 
027064     IF V EXCEEDS 31 MOVE SPACES TO PTR-8 GO TO SD4.              OR43125 
027070     MOVE VTAB (V) TO PTR-8.                                      OR43125 
027072 SD4.                                                             OR43125 
027080     WRITE FLOW-REC FROM TYP2-3 BEFORE ADVANCING 1 LINES.         OR43125 
027082     WRITE FLOW-REC FROM TYP2-2 BEFORE ADVANCING 1 LINES.         OR43125 
027084       WRITE FLOW-REC FROM TYP2-1 BEFORE ADVANCING 1 LINES.       OR43125 
027090     WRITE FLOW-REC FROM SPACRE2 BEFORE ADVANCING 1 LINES.        OR43125 
027100     MOVE END TO PTR-15.                                          OR43125 
027110     WRITE FLOW-REC FROM TYP2-4 BEFORE ADVANCING 1 LINES.         OR43125 
027120     WRITE FLOW-REC FROM SPACRE   BEFORE ADVANCING 1 LINES.       OR43125 
027140     GO TO T2.                                                    OR43125 
027150 SID5.                                                            OR43125 
027160     WRITE FLOW-REC FROM TYP3 BEFORE ADVANCING 1 LINES.           OR43125 
027170     WRITE FLOW-REC FROM TYP3-1 BEFORE ADVANCING 1 LINES.         OR43125 
027180     MOVE S-SEQ TO TP3-7.                                         OR43125 
027190     MOVE "IF" TO TP3-9.                                          OR43125 
027200     MOVE "GO" TO TP3-11.                                         OR43125 
027210     GO TO LKUP.                                                  OR43125 
027220 SID6. IF INVIND EQUALS 1 GO TO SID7.                             OR43125 
027230     MOVE BALTST (P) TO TP3-13.                                   OR43125 
027240     MOVE PGTST (P) TO SPC3-3.                                    OR43125 
027290     WRITE FLOW-REC FROM SPACRE3  BEFORE ADVANCING 1 LINES.       OR43125 
027300     WRITE FLOW-REC FROM SPACRE   BEFORE ADVANCING 1 LINES.       OR43125 
027310     WRITE FLOW-REC FROM SPACRE4  BEFORE ADVANCING 1 LINES.       OR43125 
027330     GO TO T2.                                                    OR43125 
027340 SID7. MOVE ZEROS TO INVIND.                                      OR43125 
027350     MOVE SPACES TO TP3-13.                                       OR43125 
027360     MOVE ZEROS TO SPC3-3.                                        OR43125 
027370     GO TO SA.                                                    OR43125 
027400 SID8.                                                            OR43125 
027410     WRITE FLOW-REC FROM TYP4 BEFORE ADVANCING 1 LINES.           OR43125 
027420     WRITE FLOW-REC FROM TYP4-1 BEFORE ADVANCING 1 LINES.         OR43125 
027430     MOVE S-SEQ TO TP4-5.                                         OR43125 
027440     MOVE "GO" TO TP4-7.                                          OR43125 
027442     MOVE SCH-1 TO SCH-2.                                         OR43125 
027450     GO TO LKUP.                                                  OR43125 
027460 SID9. IF INVIND EQUALS 1 GO TO SID10.                            OR43125 
027470     MOVE BALTST (P) TO TP4-9.                                    OR43125 
027480     MOVE PGTST (P) TO TP4-17.                                    OR43125 
027490 SB. MOVE S-STA TO TP4-11.                                        OR43125 
027500     WRITE FLOW-REC FROM TYP4-2 BEFORE ADVANCING 1 LINES.         OR43125 
027510     WRITE FLOW-REC FROM TYP4-1 BEFORE ADVANCING 1 LINES.         OR43125 
027520     WRITE FLOW-REC FROM TYP4 BEFORE ADVANCING 1 LINES.           OR43125 
027522     MOVE SPACES TO TP4-15.                                       OR43125 
027530     WRITE FLOW-REC FROM SPACRE5 BEFORE ADVANCING 1 LINES.        OR43125 
027532     MOVE "." TO TP4-15.                                          OR43125 
027540     MOVE ZEROS TO SPCT.                                          OR43125 
027550 SC. WRITE FLOW-REC FROM PGF-1 BEFORE ADVANCING 1 LINES.          OR43125 
027560     ADD 1 TO SPCT.                                               OR43125 
027570     IF SPCT EQUALS 3 GO TO T2.                                   OR43125 
027580     GO TO SC.                                                    OR43125 
027590 SID10.                                                           OR43125 
027600     MOVE ZEROS TO INVIND.                                        OR43125 
027610     MOVE SPACES TO TP4-9.                                        OR43125 
027620     MOVE ZEROS TO TP4-17.                                        OR43125 
027630     GO TO SB.                                                    OR43125 
028010 S7D1.                                                            OR43125 
028020     MOVE "    ..........." TO A357.                              OR43125 
028030     MOVE "   .         ."  TO D357.                              OR43125 
028040     GO TO LKUP.                                                  OR43125 
028041 S7D2. MOVE " .         .  " TO D357.                             OR43125 
028043     MOVE "...........    " TO A357.                              OR43125 
028045     GO TO S7.                                                    OR43125 
028050 S5D1.                                                            OR43125 
028060     MOVE "  ...........  " TO A357.                              OR43125 
028070     MOVE "  .         . "  TO D357.                              OR43125 
028080     GO TO LKUP.                                                  OR43125 
028090 S5D2.                                                            OR43125 
028100     WRITE FLOW-REC FROM T357A BEFORE ADVANCING 1 LINES.          OR43125 
028110     WRITE FLOW-REC FROM T357B BEFORE ADVANCING 1 LINES.          OR43125 
028120     MOVE S-SEQ TO G357.                                          OR43125 
028130     IF SDX-1 EQUALS ZERO GO TO S5D3.                             OR43125 
028140     IF SDX-1 EXCEEDS 31 GO TO S5D3.                              OR43125 
028150     MOVE SDX-1 TO V.                                             OR43125 
028160     MOVE VTAB (V) TO J357.                                       OR43125 
028170 S5D3. IF INVIND EQUALS 1 GO TO S5D5.                             OR43125 
028180     MOVE BALTST (P) TO L357.                                     OR43125 
028190     MOVE PGTST (P) TO SPC3-3.                                            
028200 S5D4. MOVE S-STA TO SPC4-2.                                      OR43125 
028210     WRITE FLOW-REC FROM T357C BEFORE ADVANCING 1 LINES.          OR43125 
028212     IF STYPE EQUALS 07 GO TO S7D2.                               OR43125 
028220 S7. WRITE FLOW-REC FROM T357B BEFORE ADVANCING 1 LINES.          OR43125 
028230     WRITE FLOW-REC FROM T357A BEFORE ADVANCING 1 LINES.          OR43125 
028240     WRITE FLOW-REC FROM SPACRE3 BEFORE ADVANCING 1 LINES.        OR43125 
028250     WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
028260     WRITE FLOW-REC FROM SPACRE4 BEFORE ADVANCING 1 LINES.        OR43125 
028270     GO TO T2.                                                    OR43125 
028280 S5D5. MOVE ZEROS TO INVIND.                                      OR43125 
028290     MOVE SPACES TO L357.                                         OR43125 
028300     MOVE ZEROS TO SPC3-3.                                        OR43125 
028310     GO TO S5D4.                                                  OR43125 
029010 S10A. MOVE "  ...........  " TO TA01.                            OR43125 
029020     MOVE   "  .         . "  TO TA02.                            OR43125 
029030     GO TO S10B.                                                  OR43125 
029040 S11A. MOVE "    ..........." TO TA01.                            OR43125 
029050     MOVE   "   .         ."  TO TA02.                            OR43125 
029060     GO TO S10B.                                                  OR43125 
029070 S12A. MOVE "    .........  " TO TA01.                            OR43125 
029080     MOVE   "   .        . "  TO TA02.                            OR43125 
029090     GO TO S10B.                                                  OR43125 
029100 S11B. MOVE " .         .  "  TO TA02.                            OR43125 
029110     MOVE   "...........    " TO TA01.                            OR43125 
029120     GO TO S10C.                                                  OR43125 
029130 S12B. MOVE " .          . "  TO TA02.                            OR43125 
029140     MOVE   ".............  " TO TA01.                            OR43125 
029150     GO TO S10C.                                                          
029160 S10B.                                                            OR43125 
029170     WRITE FLOW-REC FROM TENA BEFORE ADVANCING 1 LINES.           OR43125 
029180     WRITE FLOW-REC FROM TENB BEFORE ADVANCING 1 LINES.           OR43125 
029190     MOVE S-SEQ TO TA03.                                          OR43125 
029200     MOVE S-STA TO TA07.                                          OR43125 
029202     IF SDX-1 EQUALS ZERO MOVE "UNKNOWN " TO TA05.                OR43125 
029204     IF SDX-1 EXCEEDS 31 MOVE "UNKNOWN " TO TA05.                 OR43125 
029206     MOVE SDX-1 TO V.                                             OR43125 
029208     MOVE VTAB (V) TO TA05.                                       OR43125 
029210     WRITE FLOW-REC FROM TENC BEFORE ADVANCING 1 LINES.           OR43125 
029220     IF STYPE EQUALS 11 GO TO S11B.                               OR43125 
029230     IF STYPE EQUALS 12 GO TO S12B.                               OR43125 
029240 S10C. WRITE FLOW-REC FROM TENB BEFORE ADVANCING 1 LINES.         OR43125 
029250     WRITE FLOW-REC FROM TENA BEFORE ADVANCING 1 LINES.           OR43125 
029260     MOVE ZEROS TO SPCT GO TO PDI2.                               OR43125 
030010 S6DA. WRITE FLOW-REC FROM S06A BEFORE ADVANCING 1 LINES.         OR43125 
030020     WRITE FLOW-REC FROM S06B BEFORE ADVANCING 1 LINES.           OR43125 
030030     MOVE S-SEQ TO S6G.                                           OR43125 
030040     MOVE S-STA TO S6R.                                           OR43125 
030050     MOVE "GO" TO S6L.                                            OR43125 
030060     MOVE 0 TO LKIND.                                             OR43125 
030070     GO TO LKUP.                                                  OR43125 
030080 S6DB. IF INVIND EQUALS 1 GO TO S70.                              OR43125 
030090     IF LKIND EQUALS 1 GO TO S6DC.                                OR43125 
030100     MOVE PGTST (P) TO SPC6-5.                                    OR43125 
030110     MOVE BALTST (P) TO S6N.                                      OR43125 
030112     MOVE SCH-1 TO L.                                                     
030120     MOVE 1 TO LKIND GO TO L1.                                    OR43125 
030130 S6DC. MOVE PGTST (P) TO SPC6-2.                                  OR43125 
030140     MOVE BALTST (P) TO S6J.                                      OR43125 
030150 S6. WRITE FLOW-REC FROM S06C BEFORE ADVANCING 1 LINES.           OR43125 
030160     WRITE FLOW-REC FROM S06B BEFORE ADVANCING 1 LINES.           OR43125 
030170     WRITE FLOW-REC FROM S06A BEFORE ADVANCING 1 LINES.           OR43125 
030180     WRITE FLOW-REC FROM SPACRE6 BEFORE ADVANCING 1 LINES.        OR43125 
030190     WRITE FLOW-REC FROM SPACRE  BEFORE ADVANCING 1 LINES.        OR43125 
030200     WRITE FLOW-REC FROM SPACRE  BEFORE ADVANCING 1 LINES.        OR43125 
030210     GO TO T2.                                                    OR43125 
030215 S70.                                                                     
050010 LKUP. MOVE PLOTCHAR TO COBSCAN.                                  OR43125 
050015     MOVE ZEROS TO LKIND MOVE ZEROS TO INVIND.                            
050030 L1. IF CHAR (L) EQUALS SPACES GO TO L3.                          OR43125 
050040     MOVE ZEROS TO T.                                             OR43125 
050050     MOVE SPACES TO TABHOLD.                                      OR43125 
050060 L2. ADD 1 TO T.                                                  OR43125 
050070     IF T EXCEEDS 10 GO TO L4.                                            
050080     IF CHAR (L) EQUALS SPACES GO TO L7.                          OR43125 
050090     MOVE CHAR (L) TO TABHD (T).                                  OR43125 
050092     ADD 1 TO L.                                                  OR43125 
050100     GO TO L2.                                                    OR43125 
050110 L3. ADD 1 TO L.                                                  OR43125 
050120     IF L EXCEEDS 72 MOVE 1 TO INVIND GO TO L6.                   OR43125 
050130     GO TO L1.                                                    OR43125 
050140 L4. MOVE 001 TO P.                                               OR43125 
050150 L5. IF TABHOLD EQUALS PARTST (P) GO TO L6.                       OR43125 
050160     ADD 1 TO P.                                                  OR43125 
050170     IF P EXCEEDS 800 MOVE 1 TO INVIND GO TO L6.                  OR43125 
050180     GO TO L5.                                                    OR43125 
050190 L6. IF STYPE EQUALS 03 GO TO SID6.                               OR43125 
050200     IF STYPE EQUALS 04 GO TO SID9.                               OR43125 
050210     IF STYPE EQUALS 05 OR 07 GO TO S5D2.                         OR43125 
050220     IF STYPE EQUALS 06 GO TO S6DB.                               OR43125 
050230     IF STYPE EQUALS 13 GO TO ALP2.                               OR43125 
050232     IF SDX-1 EQUALS 12 GO TO ALP2.                               OR43125 
050240     GO TO SID4.                                                  OR43125 
050250 L7. SUBTRACT 1 FROM L.                                           OR43125 
050260     IF CHAR (L) EQUALS "." GO TO L4.                             OR43125 
050270     MOVE "." TO TABHD (T).                                       OR43125 
050280     GO TO L4.                                                    OR43125 
060010 ALT1. MOVE "..." TO PATA.                                        OR43125 
060020     MOVE SPACES TO PATB.                                         OR43125 
060030     MOVE SPACES TO PATD.                                         OR43125 
060040     MOVE ".   ." TO PA2A.                                        OR43125 
060050     MOVE SPACES TO PA2B.                                         OR43125 
060060     MOVE SPACES TO PA2D.                                         OR43125 
060070     MOVE ". " TO PA3B.                                           OR43125 
060080     MOVE " ." TO PA3D.                                           OR43125 
060090     MOVE "-----" TO PA3E.                                        OR43125 
060100     MOVE "--" TO PA3F.                                           OR43125 
060110     MOVE ".--" TO PA3J.                                          OR43125 
060120     GO TO ALP1.                                                  OR43125 
060200 PFM1. IF SCH-1 EQUALS ZERO GO TO PFM3.                           OR43125 
060210     MOVE "..." TO PATA.                                          OR43125 
060220     MOVE ".   ." TO PA2A.                                        OR43125 
060230     MOVE ". " TO PA3B.                                           OR43125 
060240     MOVE " ." TO PA3D.                                           OR43125 
060250     MOVE "-----" TO PA3E.                                        OR43125 
060260 PFM2. MOVE ".." TO PATB.                                         OR43125 
060270     MOVE ".." TO PATD.                                           OR43125 
060280     MOVE ". " TO PA2B.                                           OR43125 
060290     MOVE " ." TO PA2D.                                           OR43125 
060300     MOVE ". " TO PA3F.                                           OR43125 
060310     MOVE ". ." TO PA3J.                                          OR43125 
060320     GO TO ALP1.                                                  OR43125 
060330 PFM3. MOVE SPACES TO PATA.                                       OR43125 
060340     MOVE SPACES TO PA2A.                                         OR43125 
060350     MOVE SPACES TO PA3B.                                         OR43125 
060360     MOVE SPACES TO PA3D.                                         OR43125 
060370     MOVE SPACES TO PA3E.                                         OR43125 
060380     MOVE SPACES TO PA3C.                                         OR43125 
060390     GO TO PFM2.                                                  OR43125 
060400 ALP1. WRITE FLOW-REC FROM PALT1 BEFORE ADVANCING 1 LINES.        OR43125 
060410     WRITE FLOW-REC FROM PALT2 BEFORE ADVANCING 1 LINES.          OR43125 
060420     MOVE S-SEQ TO PA3A.                                          OR43125 
060430     MOVE S-STA TO PA3N.                                          OR43125 
060440     GO TO LKUP.                                                  OR43125 
060450 ALP2. IF INVIND EQUALS 1 GO TO AL.                               OR43125 
060460     IF LKIND EQUALS 1 GO TO ALP3.                                OR43125 
060470     MOVE PGTST (P) TO SPC6-5.                                    OR43125 
060480     MOVE PGTST (P) TO TP4-17.                                    OR43125 
060490     MOVE BALTST (P) TO PA3L.                                     OR43125 
060500     IF SCH-1 EQUALS ZERO MOVE ZEROS TO SPC6-5 GO TO AL.          OR43125 
060510     MOVE 1 TO LKIND.                                             OR43125 
060520     MOVE SCH-1 TO L.                                             OR43125 
060530     GO TO L1.                                                    OR43125 
060535 ALP3. IF INVIND EQUALS 1 GO TO AL.                               OR43125 
060540 ALP4. MOVE PGTST (P) TO SPC6-2.                                  OR43125 
060550     MOVE BALTST (P) TO PA3C.                                     OR43125 
060560 AL. IF STYPE EQUALS 13 MOVE "PERFORM" TO PA3H ELSE               OR43125 
060570     MOVE "ALTER   " TO PA3H.                                     OR43125 
060590     WRITE FLOW-REC FROM PALT2 BEFORE ADVANCING 1 LINES.          OR43125 
060600     WRITE FLOW-REC FROM PALT1 BEFORE ADVANCING 1 LINES.          OR43125 
060610     IF SCH-1  EQUALS ZERO WRITE FLOW-REC FROM SPACRE5            OR43125 
060620     BEFORE ADVANCING 1 LINES GO TO ALP5.                         OR43125 
060630     WRITE FLOW-REC FROM SPACRE6 BEFORE ADVANCING 1 LINES.        OR43125 
060640 ALP5. WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.       OR43125 
060650     WRITE FLOW-REC FROM SPACRE BEFORE ADVANCING 1 LINES.         OR43125 
060660     GO TO T2.                                                    OR43125 
080010 FINISH.                                                          OR43125 
080020     CLOSE PLOT-FILE.                                             OR43125 
080030     CLOSE FLOW-FILE.                                             OR43125 
080040 DM. STOP RUN.                                                    OR43125 
099010 END-OF-JOB.                                                      OR43125 
