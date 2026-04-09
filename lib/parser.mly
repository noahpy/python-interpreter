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
%token <int>    SPACES    (* number of spaces — consumed by indent buffer *)

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
   ============================================================

   The indent buffer emits:
   - NEWLINE at the end of each content line (statement separator)
   - INDENT / DEDENT for indentation changes
   - Mid-line SPACES are stripped and never reach the parser

   Compound statements (if/while/for/def) consume a block which
   starts with NEWLINE INDENT and ends with DEDENT. They do NOT
   require a trailing NEWLINE — the block's DEDENT is sufficient
   to terminate them. Simple statements require NEWLINE after them.
*)

(* A program is a list of statements, then EOF *)
program:
  | s = stmts; EOF  { s }

(* Statement list — used at top level and inside blocks.
   Simple statements are terminated by NEWLINE.
   Compound statements are self-terminating (end with DEDENT). *)
stmts:
  | (* empty *)                                     { [] }
  | NEWLINE; rest = stmts                           { rest }
  | s = simple_stmt; NEWLINE; rest = stmts          { s :: rest }
  | s = simple_stmt                                 { [s] }
  | s = compound_stmt; rest = stmts                 { s :: rest }

(* ---- SIMPLE STATEMENTS ---- *)

simple_stmt:
  | name = IDENT; EQUALS; x = expr   { Assign (name, x) }
  | RETURN; e = option(expr)          { match e with Some(e) -> Return e | _ -> Return (Value(Ntwo)) }
  | PASS                              { Pass }
  | e = expr                          { Expr e }

(* ---- COMPOUND STATEMENTS ---- *)

compound_stmt:
  | DEF; name = IDENT;
    LPAREN; params = separated_list(COMMA, IDENT); RPAREN;
    COLON; body = block
    { FunDef (name, params, body) }

  | IF; cond = expr; COLON;
    then_body = block;
    else_body = else_clause
    { If (cond, then_body, else_body) }

  | WHILE; cond = expr; COLON;
    body = block
    { While (cond, body) }

  | FOR; var = IDENT; IN; iter = expr; COLON;
    body = block
    { For (var, iter, body) }

(* A block is NEWLINE, INDENT, statements, DEDENT *)
block:
  | NEWLINE; INDENT; s = stmts; DEDENT   { s }

(* else / elif chains *)
else_clause:
  | (* empty *)                                     { [] }
  | ELSE; COLON; body = block                       { body }
  | ELIF; cond = expr; COLON;
    body = block; rest = else_clause
    { [If (cond, body, rest)] }

(* ---- EXPRESSIONS ---- *)
(* Note: mid-line SPACES are stripped by the indent buffer,
   so no option(SPACES) is needed anywhere in expression rules. *)

expr:
  | e = values                                                             { e }
  | name = IDENT                                                           { Var_Ref name }
  | name = IDENT; LPAREN;
    args = separated_list(COMMA, expr); RPAREN                             { Func_App(name, args) }
  | a = expr; PLUS; b = expr                                               { Bin_Exp (a, Add, b) }
  | a = expr; MINUS; b = expr                                              { Bin_Exp (a, Sub, b) }
  | a = expr; STAR; b = expr                                               { Bin_Exp (a, Mul, b) }
  | a = expr; SLASH; b = expr                                              { Bin_Exp (a, Div, b) }
  | a = expr; PERCENT; b = expr                                            { Bin_Exp (a, Mod, b) }
  | a = expr; AND; b = expr                                                { Bin_Exp (a, And, b) }
  | a = expr; OR; b = expr                                                 { Bin_Exp (a, Or, b) }
  | a = expr; EQEQ; b = expr                                              { Bin_Exp (a, Equal, b) }
  | a = expr; NEQ; b = expr                                                { Bin_Exp (a, Neq, b) }
  | a = expr; LT; b = expr                                                { Bin_Exp (a, Less, b) }
  | a = expr; GT; b = expr                                                 { Bin_Exp (a, Greater, b) }
  | a = expr; LTE; b = expr                                               { Bin_Exp (a, Leq, b) }
  | a = expr; GTE; b = expr                                               { Bin_Exp (a, Geq, b) }
  | NOT; e = expr                                                          { Bin_Exp (Value(BoolV true), Neq, e) }
  | LBRACKET; elts = separated_list(COMMA, expr); RBRACKET                { ListE elts }
  | LPAREN; e = expr; RPAREN                                              { e }
  | a = expr; LBRACKET; i = expr; RBRACKET                                 { AccessE(a, i) }
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
  | MINUS; n = INT %prec UMINUS                    { Value (IntV (-n)) }
  | MINUS; f = FLOAT %prec UMINUS                  { Value (FloatV (-.f)) }

(* dict_entry: *)
(*   | k = expr; COLON; v = expr                      { (k, v) } *)
