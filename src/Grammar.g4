grammar Grammar;

@lexer::members {
    int nesting = 0;
}

IGNORE_NEWLINE: '\n' {nesting > 0}? -> skip;
NL:             '\n';

stmtend:        NL+ | ';' NL*;

moduleFile:     (NL | extdecl | importStmt)+;

programFile:    (NL | function | extdecl | importStmt | declaration stmtend)+;

function:       'def' fndecl (NL+ | 'do' NL*) statements 'end'
        ;

extdecl:        'declare' fndecl stmtend
       ;

importStmt:     'import' name=ID stmtend
          ;

fndecl:         name=ID '(' arglist variadic='...'? ')' (':' type)?
      ;

statements:     (statement stmtend)* statement stmtend?
          ;

statement:      expression
         |      declaration
         |      assignment
         |      returnStmt
         |      conditional
         |      whileLoop
         |      forLoop
         ;

conditional:    'if' condition=expression (NL+ | 'then' NL*) trueBranch=statements ('else' NL* falseBranch=statements)? 'end'
           ;

whileLoop:      'while' condition=expression (NL+ | 'do' NL*) statements 'end'
         ;

forLoop:        'for' var=ID 'from' from=expression op=('to' | 'downto') to=expression (NL+ | 'do' NL*) statements 'end'
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

LPAREN : '(' {nesting++;} ;
RPAREN : ')' {nesting--;} ;
LBRACK : '[' {nesting++;} ;
RBRACK : ']' {nesting--;} ;

WS:             [ \t]+ -> skip;
