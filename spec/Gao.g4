grammar Gao;

pr: decl+;

decl: varDecl | functionDecl;

varDecl: varPrefix (':' data_type)? '=' expr ';';

functionDecl: 'fn' Id '(' (funcParam (',' funcParam)* )? ')' (':' data_type)? cmpStmt;

funcParam: Id ':' data_type;

cmpStmt: '{' stmt* '}';

stmt: functionCallStmt | returnStmt | varDeclStmt | assignStmt;

functionCallStmt: funcCallExpr ';';

returnStmt: 'return' expr? ';';

varDeclStmt: varDecl;

assignStmt: Id '=' expr ';';

varPrefix: 'let' | 'var';

data_type: 'int' | 'bool';

expr: logicOrExpr;

logicOrExpr: logicAndExpr ('||' logicAndExpr)*;

logicAndExpr: equalityExpr ('&&' equalityExpr)*;

equalityExpr: relationalExpr (('==' | '!=') relationalExpr)*;

relationalExpr: addSubExpr (('<' | '>' | '<=' | '>=') addSubExpr)*;

addSubExpr: mulDivExpr (('+' | '-') mulDivExpr)*;

mulDivExpr: primaryExpr (('*' | '/' | '%') primaryExpr)*;

primaryExpr: unaryExpr | groupExpr | literalExpr | refExpr | funcCallExpr;

unaryExpr: ('!' | '-') primaryExpr;

groupExpr: '(' expr ')';

literalExpr: intLiteral | boolLiteral;

refExpr: Id;

funcCallExpr: Id '(' (expr (',' expr)*)* ')';

intLiteral: IntConst;

boolLiteral: 'true' | 'false';

IntConst: Digit+;

Id: Nondigit (Nondigit | Digit)*;

Digit: [0-9];

Nondigit: [a-zA-Z_];