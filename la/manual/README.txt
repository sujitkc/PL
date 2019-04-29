MODULAR DESIGN
Software systems grow to be significantly complex. Except for the most trivial
systems, it is usually imposible to develop the entire system as a monolithic
piece. Even if we succeed to do so, such systems are very difficult to understand
and maintain for those who have to work with this piece of code. Therefore, it
is an essential engineering practice to decompose the problem so that the
invidual sub-problems can be solved separately. Another reason for modularising
code is to facilitate re-use and avoid rework. This means that common pieces of
code should be lifted out of various modules and placed in a separate module, 
which then can be used by several modules as required.

To complete the solution
all individual solutions must come together to become parts of a single solution
at the end. Therefore, they must interact through clearly defined interfaces.

The lexical analyser is composed of a number of modules, viz.
- Mystream: Implements the input stream
- State: Implements the return type for the scanners
  In: Implements the scanner for in keyword
- Id: Implements the scanner for identifiers
- Num: Implements the scanner for decimal numbers
- Lexer: Implements the top-level lexical analyser

UNIT TESTING
After a module is developed, it must be tested before it is integrated with
other modules. As you will find here, each important module <mod>.ml in the given code
comes with a testing module test_<mod>.ml.


HOW TO ADD MORE SCANNERS TO THE LEXER
This is done in two main steps.
1. Implementing the scanner for Tok.
2. Integrating with lexer.

Implementing the Scanner for Tok
================================
- Add the scanner files for Tok: tok.mli and tok.ml. Copy the contents of an existing scanner file to these to begin with. For example, id.mli -> tok.mli, id.ml -> tok.ml.
- Modify the contents of the files tok.mli and tok.ml to implement the FSA for Tok.
  * tok.mli contains a function declaration for a scanner function tok.
  * tok.ml contains a function tok to represent the scanner FSA for Tok.
- Unit Testing:
  * Add test_tok.ml file and modify the testing code appropriately by naming the testing function appropriately, and having appropriate test input strings in the list n of test case.
  * Makefile: Add the following build rules to the Makefile to add Tok into the lexer.
  tok.cmx, tok.cmi, test_tok.cmx, tok.
  * Run the unit tests as follows:
    make tok
    ./tok
Once you are satisfied with the implementation of the individual scanner, you are all set to add it to the main lexer.

Integrating with Lexer
======================
- Add a token type TOK to the token variant type on the top of the lexer.mli and lexer.ml file.
- In lexer.ml, add Tok.tok as one of the scanners in the scanners list.
  * As in: 
    scanners = [ 
      (NUM(0.), State.State(Num.num));
      (IN, State.State(In.keywd_in));
      (ID(""), State.State(Id.id));
      (TOK, State.State(Tok.tok))
    ]
   Please note that the position that you choose for TOK in the above list has implication on how the lexer identifies the tokens. Conflicts between two token types are resolved in favour of the one appearing earlier in the list above.

- Makefile: Following rules have to be modified (i.e. appropriate dependencies have to be added to the rules): lexer.cmx, lexer.cmi, test_lexer.cmx, lexer, clean.
- System testing:
  * Add appropriate test cases in the test_lexer.ml file.
  * Build the whole system and test as follows:
  make lexer
  ./lexer

- In case the build process gets stuck, please clean and build as follows:
  make clean
  make lexer
