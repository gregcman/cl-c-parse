## Exact C11 Parser in Common Lisp (No Preprocessor) (Also Includes a YACC and Lex clone)

## Usage

1. Set up the system. 
This generates esrap-liquid packrat parser rules for the lexer, ripped from the [exact lex specification](http://www.quut.com/c/ANSI-C-grammar-y.html), 
and yacc rules ripped from the [exact yacc speicification](http://www.quut.com/c/ANSI-C-grammar-l-2011.html). See yacc.txt and lex.txt. 

```
(c-parse::start-up)
=> ...WARNING: 2 Shift/Reduce, 0 Reduce/Reduce conflicts
```
The warning is a result of the C grammar having an ambiguity with regards to YACC, but this is expected.

2. Parse your C code
```
(c-parse::c-parse c-parse::*c-test-file*)
=> [very long CST tree], ("Hash" "Node" "Entry" "ReturnCode")
```

```
(c-parse::c-parse 
"int main()
{
   // printf() displays the string inside quotation
   printf(\"Hello, World!\");
   return 0;
}")
=> ((:EXTERNAL_DECLARATION 0
  (:FUNCTION_DEFINITION 1
   (:DECLARATION_SPECIFIERS 3 (:TYPE_SPECIFIER 3 "int"<0,3>))
   (:DECLARATOR 1
    (:DIRECT_DECLARATOR 12 (:DIRECT_DECLARATOR 0 "main"<4,8>) "("<8,9>
     ")"<9,10>))
   (:COMPOUND_STATEMENT 1 "{"<11,12>
    (:BLOCK_ITEM_LIST 1
     (:BLOCK_ITEM_LIST 0
      (:BLOCK_ITEM 1
       (:STATEMENT 2
        (:EXPRESSION_STATEMENT 1
         (:EXPRESSION 0
          (:ASSIGNMENT_EXPRESSION 0
           (:CONDITIONAL_EXPRESSION 0
            (:LOGICAL_OR_EXPRESSION 0
             (:LOGICAL_AND_EXPRESSION 0
              (:INCLUSIVE_OR_EXPRESSION 0
               (:EXCLUSIVE_OR_EXPRESSION 0
                (:AND_EXPRESSION 0
                 (:EQUALITY_EXPRESSION 0
                  (:RELATIONAL_EXPRESSION 0
                   (:SHIFT_EXPRESSION 0
                    (:ADDITIVE_EXPRESSION 0
                     (:MULTIPLICATIVE_EXPRESSION 0
                      (:CAST_EXPRESSION 0
                       (:UNARY_EXPRESSION 0
                        (:POSTFIX_EXPRESSION 3
                         (:POSTFIX_EXPRESSION 0
                          (:PRIMARY_EXPRESSION 0 "printf"<68,74>))
                         "("<74,75>
                         (:ARGUMENT_EXPRESSION_LIST 0
                          (:ASSIGNMENT_EXPRESSION 0
                           (:CONDITIONAL_EXPRESSION 0
                            (:LOGICAL_OR_EXPRESSION 0
                             (:LOGICAL_AND_EXPRESSION 0
                              (:INCLUSIVE_OR_EXPRESSION 0
                               (:EXCLUSIVE_OR_EXPRESSION 0
                                (:AND_EXPRESSION 0
                                 (:EQUALITY_EXPRESSION 0
                                  (:RELATIONAL_EXPRESSION 0
                                   (:SHIFT_EXPRESSION 0
                                    (:ADDITIVE_EXPRESSION 0
                                     (:MULTIPLICATIVE_EXPRESSION 0
                                      (:CAST_EXPRESSION 0
                                       (:UNARY_EXPRESSION 0
                                        (:POSTFIX_EXPRESSION 0
                                         (:PRIMARY_EXPRESSION 2
                                          (:STRING 0
                                           "\"Hello, World!\""<75,90>))))))))))))))))))
                         ")"<90,91>))))))))))))))))
         ";"<91,92>))))
     (:BLOCK_ITEM 1
      (:STATEMENT 5
       (:JUMP_STATEMENT 4 "return"<96,102>
        (:EXPRESSION 0
         (:ASSIGNMENT_EXPRESSION 0
          (:CONDITIONAL_EXPRESSION 0
           (:LOGICAL_OR_EXPRESSION 0
            (:LOGICAL_AND_EXPRESSION 0
             (:INCLUSIVE_OR_EXPRESSION 0
              (:EXCLUSIVE_OR_EXPRESSION 0
               (:AND_EXPRESSION 0
                (:EQUALITY_EXPRESSION 0
                 (:RELATIONAL_EXPRESSION 0
                  (:SHIFT_EXPRESSION 0
                   (:ADDITIVE_EXPRESSION 0
                    (:MULTIPLICATIVE_EXPRESSION 0
                     (:CAST_EXPRESSION 0
                      (:UNARY_EXPRESSION 0
                       (:POSTFIX_EXPRESSION 0
                        (:PRIMARY_EXPRESSION 1
                         (:CONSTANT 0 "0"<103,104>))))))))))))))))))
        ";"<104,105>))))
    "}"<106,107>)))), NIL
```

3. Print your CST 

```
(c-parse::print-csts *)
=>
 int
  main ( )
  {
  
   
     printf ( "Hello, World!" ) ;
    return 0 ; }

```

## Other Features

This project was part of a larger effor to try and port emacs from C to another language. 

So it has features for:
- Caching the results of the parsing in a "copy" of the filesystem
- Checking the size of the generated files in a directory,
- Parsing the C AST specification format for pycparser, and creating structure definitions

However these are unpolished.
