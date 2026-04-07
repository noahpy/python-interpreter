(* parser.mly — The Menhir grammar for our Python subset.

   STRUCTURE OF A .mly FILE:
   =========================

   %{  ... header ...  %}     OCaml code copied to the top of the generated
                              parser.ml. Use for imports and helpers.

   %token <type> NAME         Token declarations. Each token the lexer can
   %token NAME                return must be declared here. Tokens can carry
                              a value (like <int>) or be bare keywords.

   %left NAME                 Precedence and associativity declarations.
   %right NAME                Lower lines = tighter binding. These resolve
   %nonassoc NAME             shift/reduce conflicts in the grammar.

   %start <type> rulename     Which rule is the entry point, and what
                              OCaml type it produces.

   %%                         Separator — everything below is grammar rules.

   rulename:                  A grammar rule. Left side is the rule name.
     | pattern { action }     Right side is a sequence of tokens and other
     | pattern { action }     rules. Actions (in braces) build the AST.
     ;

   KEY CONCEPTS:
   - `$1`, `$2`, ... refer to matched components (ocamlyacc style).
   - `x = rule` binds the result of `rule` to variable `x` (Menhir style).
   - `separated_list(SEP, rule)` is a Menhir built-in for comma-separated
     lists, option lists, etc. — saves a lot of boilerplate.
   - `option(rule)` matches zero or one occurrence.
   - `list(rule)` matches zero or more.
   - `nonempty_list(rule)` matches one or more.
*)

%{
  open Ast  (* so we can use our AST constructors directly *)
%}

(* ---- TOKEN DECLARATIONS ---- *)

(* Tokens that carry values *)
%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <string> IDENT
%token <int>    SPACES    (* number of spaces — used by indent wrapper *)

(* Keyword tokens *)
%token DEF RETURN IF ELIF ELSE WHILE FOR IN PASS
%token NOT AND OR TRUE FALSE NONE

(* Operator and punctuation tokens *)
%token PLUS MINUS STAR SLASH PERCENT
%token EQEQ NEQ LT GT LTE GTE NOTIN
%token EQUALS COLON COMMA
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE

(* Structural tokens *)
%token NEWLINE INDENT DEDENT EOF

(* ---- PRECEDENCE (lowest to highest) ---- *)
(* Each line binds tighter than the one above it. *)
%left OR
%left AND
%nonassoc NOT
%nonassoc EQEQ NEQ LT GT LTE GTE IN NOTIN
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS   (* unary minus binds tightest *)


(* ---- ENTRY POINT ---- *)
%start <Ast.statement list> program

%%

(* ============================================================
   GRAMMAR RULES
   ============================================================ *)

(* A program is a list of statements, then EOF *)
program:
  | stmts = stmts_with_eof; EOF { stmts }

(* ---- STATEMENTS ---- *)

stmts_with_eof:
  | (* empty *)                                       { [] }
  | NEWLINE; rest = stmts_with_eof                  { rest }
  | s = stmt; NEWLINE; rest = stmts_with_eof   { s :: rest }
  | s = stmt                                         { [s] }

stmt:
  | s = simple_stmt                         { s }
  (* | s = compound_stmt                 { s } *)

ident:
  | name = IDENT; option(SPACES) { name }

simple_stmt:
  | name = ident; EQUALS; option(SPACES); x = expr  { Assign (name, x) }
  | RETURN; option(SPACES) e = option(expr)  { match e with Some(e) -> Return e | _ -> Return (Value(Ntwo))}
  | e = expr                            { Expr e }

(* compound_stmt: *)
(*   | DEF; name = IDENT; *)
(*     LPAREN; params = separated_list(COMMA, IDENT); RPAREN; *)
(*     COLON; body = block *)
(*     { FunDef (name, params, body) } *)

(*   | IF; cond = expr; COLON; *)
(*     then_body = block; *)
(*     else_body = else_clause *)
(*     { If (cond, then_body, else_body) } *)

(*   | WHILE; cond = expr; COLON; *)
(*     body = block *)
(*     { While (cond, body) } *)

(*   | FOR; var = IDENT; IN; iter = expr; COLON; *)
(*     body = block *)
(*     { For (var, iter, body) } *)

(* A block is INDENT, one or more statements, DEDENT *)
(* block: *)
(*   | NEWLINE; INDENT; stmts = nonempty_list(stmt); DEDENT *)
(*     { stmts } *)

(* else / elif chains *)
(* else_clause: *)
(*   | (* empty *)                                     { [] } *)
(*   | ELSE; COLON; body = block                       { body } *)
(*   | ELIF; cond = expr; COLON; *)
(*     body = block; rest = else_clause *)
(*     { [If (cond, body, rest)] } *)

(* ---- EXPRESSIONS ---- *)

list_sep:
   | COMMA option(SPACES) { () }

expr:
  | e = values; option(SPACES);                                                { e }
  | name = ident                                                               { Var_Ref name }
  | name = ident; LPAREN;
    args = separated_list(COMMA, expr); RPAREN                                 { Func_App(name, args) }
  | a = expr; PLUS; option(SPACES); b = expr                            { Bin_Exp (a, Add, b) }
  | a = expr; MINUS; option(SPACES); b = expr                            { Bin_Exp (a, Sub, b) }
  | a = expr; STAR; option(SPACES); b = expr                            { Bin_Exp (a, Mul, b) }
  | a = expr; SLASH; option(SPACES); b = expr                            { Bin_Exp (a, Div, b) }
  | a = expr; AND; option(SPACES); b = expr                              { Bin_Exp (a, And, b) }
  | a = expr; OR; option(SPACES); b = expr                               { Bin_Exp (a, Or, b) }
  | a = expr; EQEQ; option(SPACES); b = expr                             { Bin_Exp (a, Equal, b) }
  | a = expr; NEQ; option(SPACES); b = expr                              { Bin_Exp (a, Neq, b) }
  | a = expr; LT; option(SPACES); b = expr                               { Bin_Exp (a, Less, b) }
  | a = expr; GT; option(SPACES); b = expr                               { Bin_Exp (a, Greater, b) }
  | a = expr; LTE; option(SPACES); b = expr                              { Bin_Exp (a, Leq, b) }
  | a = expr; GTE; option(SPACES); b = expr                              { Bin_Exp (a, Geq, b) }
  | LBRACKET; option(SPACES); elts = separated_list(list_sep, expr); RBRACKET; option(SPACES)  { ListE elts }
  | LPAREN; option(SPACES); e = expr; RPAREN; option(SPACES)                   { e }
  (* | LBRACE; entries = separated_list(COMMA, dict_entry); RBRACE *)
  (*   { DictExpr entries } *)

(* Values — the leaves of the expression tree *)
values:
  | n = INT                                        { Value (IntV n) }
  | f = FLOAT                                      { Value (FloatV f) }
  | s = STRING                                     { Value (StringV s) }
  | TRUE                                           { Value (BoolV true) }
  | FALSE                                          { Value (BoolV false) }
  | NONE                                           { Value (Ntwo) }

(* dict_entry: *)
(*   | k = expr; COLON; v = expr                      { (k, v) } *)
