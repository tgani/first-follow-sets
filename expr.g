program:
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
        basic_statement
    |   "if" "(" expression ")" stmt
    |   "if" "(" expression ")" stmt "else" stmt
    |   "while" "(" expression ")" stmt
    |   "do" stmt "while" "(" expression ")" ";"
    |   "break" ";"
    |   "return" [ expression ] ";"
    |   block
	;

lvalue:
        IDENT { "[" expression "]" }
	;

basic-stmt:
        lvalue "=" expression
    |   function-call
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
    |   postfix-expression
	;

postfix-expression:
        factor
    |   function-call
	;

function-call:
        factor "(" [ argument-list ] ")"
	;


argument-list: 
        expression { "," expression }
	;

factor:
        "(" expression ")"
    |   lvalue
    |   "NUMBER"
    |   "CHAR-LITERAL"
    |   "true"
    |   "false"
	;

