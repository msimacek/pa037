grammar Grammar;

programFile:    (function | extdecl)+;

function:       'def' fndecl statements
        ;

extdecl:        'extern' fndecl END
       ;

fndecl:         name=ID '(' arglist ')' ':' type=ID
      ;

statements:     '{' (statement END)* '}'
          |     statement END
          ;

statement:      expression                     # ExprStatement
         |      declaration                    # DeclStatement
         |      assignment                     # AssignStatement
         ;

declaration:    'var' name=ID (':' type=ID)? ('=' expression)?
           ;

assignment:     name=ID '=' expression
          ;

expression:     expression op=('*' | '/') expression # ArithExpr
          |     expression op=('+' | '-') expression # ArithExpr
          |     expression op=('<' | '<=' | '>' | '>=' | '==' | '!=') expression # ArithExpr
          |     expression op=('and' | 'or') expression # LogicExpr
          |     'not ' expression                    # NotExpr
          |     '(' expression ')'                   # Expr
          |     literal                              # Expr
          |     call                                 # Expr
          |     identifier                           # Expr
          ;

arglist:        (arg (',' arg)*)?
       ;

arg:            name=ID ':' type=ID
   ;

call:           ID '(' (expression ( ',' expression)*)? ')'
    ;

literal:        value=INTEGER             # IntegerLiteral
       |        value=('true' | 'false')  # BooleanLiteral
       ;

identifier:     ID;

INTEGER:        [0-9]+;
ID:             [a-zA-Z_][a-zA-Z_0-9]*;

END:            ';';
WS:             [ \t\r\n] -> skip;
