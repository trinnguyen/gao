grammar Gao;

pr: decl+;

decl: varDecl | functionDecl;

varDecl: varPrefix (':' data_type)? '=' expr;

functionDecl: 'fn' Id '(' (funcParam (',' funcParam)* )? ')' (':' data_type)? cmpStmt;

funcParam: Id ':' data_type;

cmpStmt: '{' stmt* '}';

stmt: exprStmt | returnStmt;

exprStmt: expr ';';

returnStmt: 'return' expr? ';';

varPrefix: 'let' | 'var';

data_type: 'int' | 'bool';

expr: literalExpr | refExpr | funcCallExpr;

literalExpr: intLiteral | boolLiteral;

refExpr: Id;

funcCallExpr: Id '(' (expr (',' expr)*)* ')';

intLiteral: IntConst;

boolLiteral: 'true' | 'false';

IntConst: Digit+;

Id: Nondigit (Nondigit | Digit)*;

Digit: [0-9];

Nondigit: [a-zA-Z_];