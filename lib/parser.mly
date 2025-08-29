// tokens
%token <float> NUMBER
%token <string> ID
%token EQUAL
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token EOF
%token LEFT_PAREN RIGHT_PAREN
%token LET VAR ASSIGN IN END

// Precedence and associativity
%left PLUS MINUS
%left STAR SLASH

// Start symbol
%start <Ast.expr> main
%%

main:
    | expr EOF {$1}

expr:
    | NUMBER {Ast.Num $1}
    | ID { Ast.Var $1 }
    | LET vardec IN expr END {
        let (id, value_expr) = $2 in
        Ast.Let(id, value_expr, $4)
    }
    | expr PLUS expr { Ast.Add($1, $3) }
    | expr MINUS expr { Ast.Sub($1, $3) }
    | expr STAR expr { Ast.Mul($1, $3) }
    | expr SLASH expr { Ast.Div($1, $3) }
    | LEFT_PAREN expr RIGHT_PAREN {$2}

vardec:
    | VAR ID ASSIGN expr { ($2, $4) }