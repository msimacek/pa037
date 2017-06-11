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

forLoop:        'for' var=ID 'from' from=expression op=('to' | 'downto') to=expression statements
       ;

returnStmt:     'return' expression?
          ;

declaration:    'var' name=ID (':' type)? ('[' arrayDim=expression ']')? ('=' initializer=expression)?
           ;

assignment:     lhs=expression '=' rhs=expression
          ;

expression:     expr=expression '[' subscript=expression ']'        # SubscriptExpr
          |     expr=expression '::' type            # CastExpr
          |     '&' expression                       # AddrExpr
          |     expression op=('*' | '/') expression # ArithExpr
          |     expression op=('+' | '-') expression # ArithExpr
          |     expression op=('<' | '<=' | '>' | '>=' | '==' | '!=') expression # ArithExpr
          |     expression op=('and' | 'or') expression # LogicExpr
          |     'not ' expression                    # NotExpr
          |     '(' expression ')'                   # ParenExpr
          |     literal                              # Expr
          |     call                                 # Expr
          |     identifier                           # Expr
          ;

arglist:        (arg (',' arg)*)?
       ;

arg:            name=ID ':' type
   ;

call:           name=ID '(' (expression ( ',' expression)*)? ')'
    ;

literal:        value=INTEGER             # IntegerLiteral
       |        value=FLOAT               # FloatLiteral
       |        value=CHAR                # CharLiteral
       |        value=STRING              # StringLiteral
       |        value=('true' | 'false')  # BooleanLiteral
       ;

type:           name=ID ptrDims
    ;

ptrDims:         '*'*;

identifier:     ID;

FLOAT:          [0-9]*[.][0-9]+;
INTEGER:        [0-9]+;
CHAR:           [']~[']['];
STRING:         ["]~["]*["];
ID:             [a-zA-Z_][a-zA-Z_0-9]*;
COMMENT:        '//' ~[\r\n]* -> skip;

END:            ';';
WS:             [ \t\r\n] -> skip;
