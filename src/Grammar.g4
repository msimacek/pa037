grammar Grammar;

programFile:    function+;

statements:     '{' statement* '}'
          |     statement
          ;

statement:      expression END                     # ExprStatement
         |      declaration END                    # DeclStatement
         |      assignment END                     # AssignStatement
         ;

declaration:    'var' ID
           ;

assignment:     ID '=' expression
          ;

expression:     expression op=('*' | '/') expression   # MulDivExpr
          |     expression op=('+' | '-') expression   # AddSubExpr
          |     '(' expression ')'                  # ParenExpr
          |     literal                             # LiteralExpr
          |     call                                # CallExpr
          |     ID                                  # IdentifierExpr
          ;

function:       'def' name=ID '(' arglist? ')' statements
        ;

arglist:        ID (',' ID)*
       ;

call:           ID '(' (expression ( ',' expression)*)? ')'
    ;

literal:        INTEGER;

INTEGER:        [0-9]+;
ID:             [a-zA-Z_][a-zA-Z_0-9]*;

END:            ';'
                | '\r'? '\n';

WS:             [ \t] -> skip;
