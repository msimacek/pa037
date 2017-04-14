grammar Grammar;

programFile:    (function | extdecl)+;

function:       'def' fndecl statements
        ;

extdecl:        'extern' fndecl END
       ;

fndecl:         name=ID '(' arglist variadic='...'? ')' (':' type)?
      ;

statements:     '{' (statement)* '}'
          ;

statement:      expression END
         |      declaration END
         |      assignment END
         |      returnStmt END
         |      conditional
         |      whileLoop
         |      forLoop
         ;

conditional:    'if' condition=expression trueBranch=statements ('else' falseBranch=statements)?
           ;

whileLoop:      'while' condition=expression statements
         ;

forLoop:        'for' var=ID 'from' from=expression 'to' to=expression statements
       ;

returnStmt:     'return' expression?
          ;

declaration:    'var' name=ID (':' type)? ('=' expression)?
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
          |     '&' ID                               # AddrExpr
          ;

arglist:        (arg (',' arg)*)?
       ;

arg:            name=ID ':' type
   ;

call:           name=ID '(' (expression ( ',' expression)*)? ')'
    ;

literal:        value=INTEGER             # IntegerLiteral
       |        value=CHAR                # CharLiteral
       |        value=STRING              # StringLiteral
       |        value=('true' | 'false')  # BooleanLiteral
       ;

type:           name=ID ptrDims='*'*
    ;

identifier:     ID;

INTEGER:        [0-9]+;
CHAR:           [']~[']['];
STRING:         ["]~["]*["];
ID:             [a-zA-Z_][a-zA-Z_0-9]*;
COMMENT:        '//' ~[\r\n]* -> skip;

END:            ';';
WS:             [ \t\r\n] -> skip;
