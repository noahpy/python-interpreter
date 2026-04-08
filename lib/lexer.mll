(* lexer.mll — The ocamllex lexer definition for our Python subset.

   STRUCTURE OF A .mll FILE:
   ========================

   {  ... header ...  }       OCaml code copied verbatim to the top of
                              the generated lexer.ml. Use it for imports,
                              helper functions, and open statements.

   let name = regexp          Named regular expression aliases — shortcuts
                              you can reuse in the rules below.

   rule rulename = parse      The actual lexer rules. Each rule is a function
     | pattern { action }     that takes a lexbuf and returns a token.
     | pattern { action }     Patterns are regular expressions.
     | ...                    Actions (in braces) are OCaml code that must
                              return a token from your Parser module.

   {  ... trailer ...  }      OCaml code copied to the bottom of the
                              generated file. Optional — rarely used.

   KEY CONCEPTS:
   - `lexbuf` is the implicit buffer threaded through every rule.
   - `as x` binds the matched text to a variable x.
   - `Lexing.lexeme lexbuf` gets the full matched string.
   - Calling another rule (e.g., `read lexbuf`) means "skip this, lex again."
*)

{
  (* ---- HEADER: plain OCaml code ---- *)
  open Parser  (* gives us access to token constructors: INT, DEF, etc. *)

  (** Interpret Python escape sequences in a string literal. *)
  let unescape s =
    let buf = Buffer.create (String.length s) in
    let len = String.length s in
    let rec loop i =
      if i >= len then Buffer.contents buf
      else if Char.equal s.[i] '\\' && i + 1 < len then
        (match s.[i + 1] with
         | 'n'  -> Buffer.add_char buf '\n'; loop (i + 2)
         | 't'  -> Buffer.add_char buf '\t'; loop (i + 2)
         | '\\' -> Buffer.add_char buf '\\'; loop (i + 2)
         | '\'' -> Buffer.add_char buf '\''; loop (i + 2)
         | '"'  -> Buffer.add_char buf '"';  loop (i + 2)
         | 'r'  -> Buffer.add_char buf '\r'; loop (i + 2)
         | '0'  -> Buffer.add_char buf '\000'; loop (i + 2)
         | c    -> Buffer.add_char buf '\\'; Buffer.add_char buf c; loop (i + 2))
      else (Buffer.add_char buf s.[i]; loop (i + 1))
    in
    loop 0
}

(* ---- NAMED PATTERNS ---- *)
let digit  = ['0'-'9']
let alpha  = ['a'-'z' 'A'-'Z' '_']
let alnum  = alpha | digit
let space  = ' '

(* ---- LEXER RULES ---- *)
rule read = parse

  (* -- Whitespace handling --
     We emit SPACES at line start so the indent wrapper can count them.
     Mid-line spaces are just skipped. The indent wrapper sets at_line_start,
     but the lexer doesn't know about that — it always emits SPACES for
     runs of spaces at the start, and the wrapper decides what to do. *)
  | space+ as s        { SPACES (String.length s) }
  | '\t'               { SPACES 4 }

  (* -- Newlines -- *)
  | '\n'               { Lexing.new_line lexbuf; NEWLINE }

  (* -- Comments: skip everything until end of line -- *)
  | '#' [^ '\n']*      { read lexbuf }

  (* -- Keywords: checked BEFORE identifiers so "def" isn't parsed as IDENT --*)
  | "def"              { DEF }
  | "return"           { RETURN }
  | "if"               { IF }
  | "elif"             { ELIF }
  | "else"             { ELSE }
  | "while"            { WHILE }
  | "for"              { FOR }
  | "in"               { IN }
  | "not"              { NOT }
  | "and"              { AND }
  | "or"               { OR }
  | "pass"             { PASS }
  | "True"             { TRUE }
  | "False"            { FALSE }
  | "None"             { NONE }

  (* -- Literals -- *)
  | digit+ as n                       { INT (int_of_string n) }
  | digit+ '.' digit* as f            { FLOAT (float_of_string f) }
  | '"' ([^ '"' '\n']* as s) '"'      { STRING (unescape s) }
  | '\'' ([^ '\'' '\n']* as s) '\''   { STRING (unescape s) }

  (* -- Identifiers: anything starting with alpha, then alnum -- *)
  | alpha alnum* as id  { IDENT id }

  (* -- Two-character operators (must come before single-char versions) -- *)
  | "=="               { EQEQ }
  | "!="               { NEQ }
  | "<="               { LTE }
  | ">="               { GTE }
  | "not" space+ "in"  { NOTIN }

  (* -- Single-character operators and punctuation -- *)
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { STAR }
  | '/'                { SLASH }
  | '%'                { PERCENT }
  | '<'                { LT }
  | '>'                { GT }
  | '='                { EQUALS }
  | ':'                { COLON }
  | ','                { COMMA }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | '['                { LBRACKET }
  | ']'                { RBRACKET }
  | '{'                { LBRACE }
  | '}'                { RBRACE }

  (* -- End of file -- *)
  | eof                { EOF }

  (* -- Catch-all: anything else is an error -- *)
  | _ as c             { failwith (Printf.sprintf
                           "Unexpected character: '%c' at line %d"
                           c lexbuf.lex_curr_p.pos_lnum) }
