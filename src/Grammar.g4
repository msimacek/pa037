grammar Grammar;

programFile:    (function | extdecl)+;

function:       'def' name=ID '(' arglist ')' statements
        ;

extdecl:        'extern' name=ID '(' arglist ')'
       ;

statements:     '{' (statement (END statement)*)? '}'
          |     statement END
          ;

statement:      expression                     # ExprStatement
         |      declaration                    # DeclStatement
         |      assignment                     # AssignStatement
         ;

declaration:    'var' name=ID
           ;

assignment:     name=ID '=' expression
          ;

expression:     expression '*' expression   # MulExpr
          |     expression '/' expression   # DivExpr
          |     expression '+' expression   # AddExpr
          |     expression '-' expression   # SubExpr
          |     '(' expression ')'          # ParenExpr
          |     literal                     # LiteralExpr
          |     call                        # CallExpr
          |     ID                          # IdentifierExpr
          ;

arglist:        (ID (',' ID)*)?
       ;

call:           ID '(' (expression ( ',' expression)*)? ')'
    ;

literal:        value=INTEGER         # IntegerLiteral
       ;

INTEGER:        [0-9]+;
ID:             [a-zA-Z_][a-zA-Z_0-9]*;

END:            ';'
                | '\r'? '\n';

WS:             [ \t] -> skip;
