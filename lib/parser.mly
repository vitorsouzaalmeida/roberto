// tokens
%token <float> NUMBER
%token PLUS
%token EOF
%token LEFT_PAREN RIGHT_PAREN

// Precedence and associativity
%left PLUS

// Start symbol
%start <float> main
%%

main:
    | expr EOF {$1}

expr:
    | NUMBER {$1}
    | expr PLUS expr {$1 +. $3}
    | LEFT_PAREN expr RIGHT_PAREN {$2}