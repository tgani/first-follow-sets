program:
        { decl }
	;

decl:
        "var" "ID" type ";"
    |   "func" "ID" "(" [ param-list ] ")" [ type ] block
	;

param-list:
        "ID" type { "," "ID" type }
	;

type:
        "[" "NUMBER" "]" type
    |   basic-type
	;

basic-type:
        "char" | "bool" | "int" | "real"
		;

block:
    "{" { decl } stmts "}"
	;

stmts:
	{ stmt }
	 ;

stmt:
        basic-stmt ";"
    |   "if" "(" expr ")" stmt [ "else" stmt ] ";"
    |   "while" "(" expr ")" stmt ";"
    |   "do" stmt "while" "(" expr ")" ";"
    |   "break" ";"
    |   "return" [ expr ] ";"
    |   block
	;

basic-stmt:
        assign-expr
	;

assign-expr:
        expr { "=" expr }
    ;

expr:
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
        postfix-expr { "^" postfix-expr }
    ;

postfix-expr:
        primary  [  argument-list ] 
	;



argument-list: 
        "(" [ expr { "," expr } ] ")"
	;

lvalue:
        "IDENT" { "[" expr "]" }
	;

primary:
        "(" expr ")"
    |   lvalue
    |   "NUMBER"
    |   "CHAR-LITERAL"
    |   "true"
    |   "false"
    |   "BLTIN"
	;

