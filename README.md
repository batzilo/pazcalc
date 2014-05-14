#Pazcalc - A pazcal compiler

## To make the compiler:

    $ make

## Packages Needed:

   - GNU make
   - ocaml
   - camlp5
   - dosbox

## File Legend:

   - **Error.ml, Error.mli** : Error handling
   - **extend.ml** : nickie's magic
   - **Final.ml, Final.mli** : Final code generator (translates intermediate code to 8086 assembly)
   - **Hashcons.ml, Hashcons.mli** : polymorphic hash consing...
   - **Identifier.ml, Identifier.mli** : hash consed identifiers
   - **Lexer.mll** : Lexical analysis (input to ocamllex)
   - **Main.ml** : compiler's main function. command line argument checking etc
   - **Parser.mly** : Syntactic analysis (parsing), compiler's backbone
   - **SemQuad.ml, SemQuad.mli** : Semantic check functions and Quadruple intermediate code generator
   - **Symbol.ml, Symbol.mli** : Symbol Table manipulation
   - **Types.ml, Types.mli** : Data types

