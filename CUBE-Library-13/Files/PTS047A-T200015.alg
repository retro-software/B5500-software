COMMENT       INCLUDED HERE ARE THE ARRAY DECLARATIONS AND FILL         HLPC0001
              STATEMENTS FOR THE VARIABLES HLC1 TO HLC4.  THESE         HLPC0002
              VARIABLES ARE NON-LOCAL TO THE HLPICK PROCEDURE AND ARE   HLPC0003
              USED IN THE KELLOGG LIQUID ENTHALPY CORRELATION.  ;       HLPC0004
REAL ARRAY    HLC1, HLC2[0:2,0:4], HLC3[0:25] ;                         HLPC0005
INTEGER ARRAY HLC4[0:2] ;                                               HLPC0006
COMMENT       FILL THE HLC1 ARRAY ;                                     HLPC0007
              FILL HLC1[1,*] WITH 0,-120.5,-144.41423,-138.23442,       HLPC0008
              -133.04614 ;                                              HLPC0009
              FILL HLC1[2,*] WITH 0,-150.0,-150.0,-150.0,-150.0 ;       HLPC0010
COMMENT       FILL THE HLC2 ARRAY ;                                     HLPC0011
              FILL HLC2[1,*] WITH 0,0,-4722.25,-3198.2357,-3153.1499 ;  HLPC0012
              FILL HLC2[2,*] WITH 0,0,-25000.0,-19500.0,-18000.0 ;      HLPC0013
COMMENT       FILL THE HLC3 ARRAY ;                                     HLPC0014
              FILL HLC3[*] WITH  0,                                     HLPC0015
              20556.045,          68.556818,          0.036708857,      HLPC0016
              0.000020878010,     0.19305354@-6,      141.71055,        HLPC0017
              0.45013447,         0.00011845653,      0.10232359@-6,    HLPC0018
              0.24086213@-8,      0.044834296,        0.00026115507,    HLPC0019
              0.40933551@-6,      -0.39076914@-9,     -0.37304907@-11,  HLPC0020
              -0.00055553665,     -0.32234078@-5,     -0.49185676@-8,   HLPC0021
              -0.13105535@-10,    -0.92690507@-13,    0.58386104@-5,    HLPC0022
              0.29665002@-7,      0.44235535@-10,     0.50302087@-12,   HLPC0023
              0.70354043@-15 ;                                          HLPC0024
COMMENT       FILL THE HLC4 ARRAY ;                                     HLPC0025
              FILL HLC4[*] WITH 0, 4, 4 ;                               HLPC0026
