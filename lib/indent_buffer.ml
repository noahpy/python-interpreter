
(* indent_buffer.ml — Sits between the raw lexer and the parser.
   Tracks indentation levels and injects INDENT / DEDENT tokens.

   Token stream contract:
   - NEWLINE is emitted at the end of each content line (statement separator)
   - INDENT is emitted when a new line's indentation increases
   - DEDENT is emitted (possibly multiple) when indentation decreases
   - Mid-line SPACES are stripped (never reach the parser)
   - Blank lines are skipped entirely
   - At EOF, pending DEDENTs are flushed before the final EOF token *)

open Parser

type state = {
  mutable indent_stack  : int list;   (* stack of indent levels, bottom is always 0 *)
  mutable pending       : token list; (* queued tokens to emit before reading more *)
  mutable at_line_start : bool;       (* are we expecting indentation? *)
  mutable had_content   : bool;       (* have we emitted any real tokens yet? *)
}

let create () : state =
  { indent_stack = [0]; pending = []; at_line_start = true; had_content = false }

(* Pop indent levels off the stack down to [target], accumulating DEDENT tokens. *)
let pop_dedents (stack : int list) (target : int) : token list * int list =
  let rec go (acc : token list) (remaining : int list) : token list * int list =
    match remaining with
    | level :: rest when level > target -> go (DEDENT :: acc) rest
    | level :: _    when level = target -> (List.rev acc, remaining)
    | _ -> failwith "inconsistent indentation"
  in
  go [] stack

(* Build the token sequence for the start of a new content line at [width]
   indentation, with [first_tok] being the first real token on that line.
   Returns (tokens_to_emit, updated_indent_stack). *)
let line_start_tokens (state : state) (width : int) (first_tok : token)
    : token list =
  let tokens = ref [] in
  (* 1. NEWLINE separator from previous line *)
  if state.had_content then
    tokens := NEWLINE :: !tokens;
  (* 2. INDENT / DEDENT *)
  let current = List.hd state.indent_stack in
  if width > current then begin
    state.indent_stack <- width :: state.indent_stack;
    tokens := INDENT :: !tokens
  end else if width < current then begin
    let (dedents, new_stack) = pop_dedents state.indent_stack width in
    state.indent_stack <- new_stack;
    tokens := List.rev_append dedents !tokens
  end;
  (* 3. The actual first token of the line *)
  tokens := first_tok :: !tokens;
  List.rev !tokens

(* Main entry point: returns the next token for the parser. *)
let rec next_token (state : state) (lexbuf : Lexing.lexbuf) : token =
  (* Drain pending queue first *)
  match state.pending with
  | tok :: rest ->
    state.pending <- rest;
    tok
  | [] ->
    let tok = Lexer.read lexbuf in
    if state.at_line_start then
      handle_line_start state lexbuf tok 0
    else
      match tok with
      | NEWLINE ->
        (* End of a content line: mark line start, but don't emit NEWLINE yet.
           It will be emitted as a separator when the next content line begins. *)
        state.at_line_start <- true;
        next_token state lexbuf
      | EOF ->
        flush_dedents_then state EOF
      | SPACES _ ->
        (* Mid-line spaces: skip *)
        next_token state lexbuf
      | _ -> tok

(* At line start, accumulate indentation width from SPACES tokens,
   skip blank lines, and when a real token arrives, emit the
   NEWLINE + INDENT/DEDENT + token sequence. *)
and handle_line_start (state : state) (lexbuf : Lexing.lexbuf)
    (tok : token) (width : int) : token =
  match tok with
  | SPACES n ->
    (* Accumulate indentation width, then read next token *)
    let next = Lexer.read lexbuf in
    handle_line_start state lexbuf next n
  | NEWLINE ->
    (* Blank line: stay at line start, reset width *)
    let next = Lexer.read lexbuf in
    handle_line_start state lexbuf next 0
  | EOF ->
    flush_dedents_then state EOF
  | _ ->
    (* First real token on the line *)
    state.at_line_start <- false;
    state.had_content <- true;
    let tokens = line_start_tokens state width tok in
    begin match tokens with
    | first :: rest ->
      state.pending <- rest @ state.pending;
      first
    | [] ->
      (* Should not happen: line_start_tokens always includes first_tok *)
      tok
    end

(* At EOF, emit NEWLINE (if we had content) + DEDENT for each open indent + EOF. *)
and flush_dedents_then (state : state) (final : token) : token =
  let tokens = ref [] in
  if state.had_content then
    tokens := NEWLINE :: !tokens;
  state.had_content <- false;
  let rec go (stack : int list) : int list =
    match stack with
    | level :: rest when level > 0 ->
      tokens := DEDENT :: !tokens;
      go rest
    | _ -> stack
  in
  state.indent_stack <- go state.indent_stack;
  let result = List.rev !tokens in
  match result with
  | first :: rest ->
    state.pending <- rest @ [final];
    first
  | [] -> final
