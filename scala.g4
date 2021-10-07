grammar scala;

/*
 Parser Rules
 */
start : statement*;
statement:
	assignExpression
	| importStatement
	| patternMatching
	| varDecl
	| classDecl
	| classInst
	| forLoop
	| forCollection
	| whileLoop
	| ifStatement
	| functionCall
	| function
	| block;

importStatement: IMPORT MODULE_NAME;

patternMatching:
	VAR_NAME MATCH '{'
	(CASE ( (LIST '(' (NUM | '_' | '*') (',' (NUM | '_' | '*')*) ')') | VAR_NAME | STR |) '=>' statement* (BREAK)?)
	(CASE (VAR_NAME | STR | (LIST '(' (NUM | '_' | '*') (',' (NUM | '_' | '*'))* ')')) '=>' statement* (BREAK)?)*
	(CASE '_' '=>' statement* (BREAK)?)?
	'}';
varDecl:
	(VAR | VAL) VAR_NAME ':' (TYPE | ARR1 | ARR2) ('=' ((NUM | SCIENTIFIC_NUM | STR |BOOL | exp) | VAR_NAME | ARR_NAME))?
	(',' VAR_NAME ':' (TYPE | ARR1 | ARR2) ('=' ((NUM | SCIENTIFIC_NUM | STR | BOOL | exp) | VAR_NAME | ARR_NAME ))?)*;


classDecl:
	CLASS VAR_NAME (EXTENDS VAR_NAME)? (IMPLEMENTS VAR_NAME (',' VAR_NAME)*)? '(' (VAR_NAME ':' (TYPE|ARR1|ARR2) (',' VAR_NAME ':' (TYPE|ARR1|ARR2))*)? ')' '{' statement* '}'; // code doroste?

function:
	DEF VAR_NAME '(' (VAR_NAME ':' (TYPE|ARR1|ARR2) (',' VAR_NAME ':' (TYPE|ARR2|ARR1))*)? ')' ':' (TYPE)? '=>'
	'{' statement* (RETURN (VAR_NAME | NUM | exp))? '}';

classInst:
	(VAR | VAL) VAR_NAME ':' NEW VAR_NAME '(' (NUM | SCIENTIFIC_NUM | STR) (',' (NUM | SCIENTIFIC_NUM | STR))* ')';

forLoop:
	FOR '(' VAR_NAME '<-' '(' NUM ',' NUM ')' ('step' NUM)? (';' VAR_NAME '<-' '(' NUM ',' NUM ')' ('step' NUM)?)* ')'
	 '{' statement* '}';

forCollection:
	FOR '(' VAR_NAME '<-' VAR_NAME ')' '{' statement* '}';

whileLoop: 'while' '(' exp ')' '{' statement* '}';

ifStatement:
	IF '(' exp ')' '{' statement* '}'
	(ELSE IF '(' exp ')' '{' statement* '}')*
	(ELSE '{' statement* '}')?;


functionCall: VAR_NAME '(' ((exp (',' exp)*)? | STR) ')';

block: '{' statement* '}';

assignExpression: VAR_NAME ((ASSIGN_OP | '=') (exp | SCIENTIFIC_NUM))+;

exp:
	'(' exp ')'
	| exp ('**') exp //power
	| ('~') exp //unary bit op
	| ('+' | '-') exp //unary sign op
	| exp ('*' | '/' | '%' | '//') exp //multiplicative op
	| exp ('+' | '-') exp //additive
	| ('--' | '++') exp //inc/decrement
	| exp ('--' | '++')
	| exp ('<<' | '>>' | '>>>') exp //shift
	| exp ('^' | '|' | '&') exp //bitwise op
	| exp ('==' | '!=' | '<>') exp //equality op
	| exp ('>' | '<' | '>=' | '<=') exp //inequality op
	| 'not' exp
	| exp ('or' | '||' | 'and' | '&&') exp //logical or/and
	| atom;
atom: ( SIGN? (NUM | VAR_NAME | ARR_NAME)) | BOOL | functionCall; ///////kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk

/*Lexer Rules */
SCOMMENT:
	'#' ~[\r\n]* -> skip; //skip single-line comments starting with #
MCOMMENT:
	'/*' .*? '*/' -> skip; //skip multi-line comments in /*.. */ format
WS: [ \n\t\r]+ -> skip;

/*----KEYWORDS----*/

TYPE: INT | STRING | DOUBLE | FLOAT | BOOLEAN;
INT : 'Int';
STRING : 'String';
DOUBLE : 'Double';
FLOAT : 'Float';
LIST: 'List';
IMPORT: 'import';
VAR: 'var';
VAL: 'val';
CLASS: 'class';
EXTENDS: 'extends';
IMPLEMENTS: 'implements';
DEF: 'def';
NEW: 'new';
FOR: 'for';
RETURN: 'return';
IF: 'if';
ELSE: 'else'; 
MATCH: 'match';
CASE: 'case';
BREAK: 'break';
BOOL: ('true' | 'false');
BOOLEAN : 'Bool';

ASSIGN_OP:
	'*='
	| '/='
	| '%='
	| '//='
	| '+='
	| '-='
	| '**='
	| '|='
	| '&='
	| '^='
	;

fragment DIGIT: [0-9];
fragment LETTER: [a-zA-Z];
SIGN: ('-' | '+');
NUM: SIGN? DIGIT+ ([.] DIGIT+)?;
SCIENTIFIC_NUM: (NUM ('e' SIGN? DIGIT+))|('.' DIGIT+);
MODULE_NAME:
	LETTER+ (
		(('.' LETTER+)* ('.' '_'))
		| (('.' LETTER+)*('.{'((LETTER+ ' as ' LETTER+)(', ' LETTER+)*)?((LETTER+)(', ' LETTER+)+) '}' ))
		|('.' LETTER+)+
	);

ARR1: 'Array' '(' ((NUM | SCIENTIFIC_NUM | STR) (',' (NUM | SCIENTIFIC_NUM | STR))*)? ')';
ARR2: 'Array' '[' TYPE ']' ('(' DIGIT+ ')')?;
ARR_NAME : VAR_NAME '[' DIGIT+ ']';
VAR_NAME: (LETTER | '$' | '_') (DIGIT | LETTER | '$' | '_')*;
STR: '"' ~[\r\n"]* '"';



