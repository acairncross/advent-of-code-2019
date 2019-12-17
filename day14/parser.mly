%token <int> INT
%token <string> IDENT
%token COMMA
%token ARROW
%token EOF

%start <((int * string) list * (int * string)) list> reactions
%start <(int * string) list * (int * string)> reaction
%%

reactions:
  | rs = list(reaction) ; EOF
    { rs }

reaction:
  | inputs = separated_list(COMMA, chemical); ARROW; output = chemical
    { (inputs, output) }

chemical:
  | n = INT; x = IDENT
    { (n, x) };
