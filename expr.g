program:
        decls
	;

decls:
	{ decl }
	;

decl:
        "var" "ID" type";"
    |   function-definition
	;

function-definition:
    function-decl block
	;

function-decl:
    "func" "ID" "(" [ param-list ] ")" [ return-type ]
	;

param-list:
        "ID" type { "," "ID" type }
	;

block:
    "{" decls stmts "}"
	;

return-type:
	type
	;

type:
        "[" "NUMBER" "]" type
    |   basic-type
	;

basic-type:
        "char" | "bool" | "int" | "real"
		;

stmts:
	{ stmt }
	 ;

stmt:
        basic-stmt
    |   "if" "(" expression ")" stmt [ "else" stmt ]
    |   "while" "(" expression ")" stmt
    |   "do" stmt "while" "(" expression ")" ";"
    |   "break" ";"
    |   "return" [ expression ] ";"
    |   block
	;

lvalue:
        "IDENT" { "[" expression "]" }
	;

basic-stmt:
        assignment-expression
	;

assignment-expression:
        expression { "=" expression }
    ;

expression:
        join { "||" join }
	;

join:
        equality { "&&" equality }
	;

equality:
        relexp { ( "==" | "!=") relexp }
	;

relexp:
        arithexpr [ ("<" | ">" | "<=" | ">=") arithexpr ]
	;

arithexpr:
        term { ("+" | "-" ) term }
	;

term:
        unary { ("*" | "/") unary }
	;

unary:
        ("!" | "-" | "+") unary
    |   factor
	;

factor:
        postfix-expression { "^" postfix-expression }
    ;

postfix-expression:
        primary  [  argument-list ] 
	;



argument-list: 
        "(" [ expression { "," expression } ] ")"
	;

primary:
        "(" expression ")"
    |   lvalue
    |   "NUMBER"
    |   "CHAR-LITERAL"
    |   "true"
    |   "false"
    |   "BLTIN"
	;

