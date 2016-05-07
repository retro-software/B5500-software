EULER Compiler for the Burroughs B5500

An implemention for the EULER language, designed by Niklaus Wirth,
apparently while he was in residence at Stanford University (California,
US). A comment in the EULERIV source dates it to December 1964, thus
this software would have been developed originally on the B5000 at
Stanford before that system was upgraded to a B5500.

The following files were transcribed by James Fehlinger of New Jersey,
US, during March 2014, from "EULER: A Generalization of Algol and its
Formal Definition," Niklaus Wirth and Helmut Weber, Stanford University
Technical Report CS20, 27 April 1965.

EULER is also described in "EULER: A Generalization of ALGOL, and its
Formal Definition: Part I" and "--Part II", Communications of the ACM,
vol. 9, numbers 1 and 2, (January/February 1966).

EULERIV.alg_m
    Wirth's translator and interpreter for the EULER language.

GRAMMAR.card
    Card deck containing the grammar definition used by the EULERIV
    translator. This deck is processed by the SYNTAX program to punch
    tables which were then manually inserted into the EULERIV source
    program.

SAMPLE.card
    Card deck containing a sample EULER program.

SYNTAX.alg_m
    Program to process the GRAMMAR.card deck and punch cards with tables
    to be inserted into the EULERIV.alg_m translator source.

