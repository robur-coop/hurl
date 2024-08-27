{
let hex_digit_value d =
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let hexadecimal_code s =
  let rec loop acc i =
    if i < String.length s then
      let value = hex_digit_value s.[i] in
      loop (16 * acc + value) (i + 1)
    else acc in
  loop 0 0

let char_for_octal_code c d u =
  let c = 64 * (Char.code c - 48) +
           8 * (Char.code d - 48) +
               (Char.code u - 48) in
  Char.chr c

let char_for_hexadecimal_code d u =
  Char.chr (16 * (hex_digit_value d) + (hex_digit_value u))

let hurl_escape str =
  let n = ref 0 in
  for i = 0 to String.length str - 1 do
    n := !n + begin match str.[i] with
      | '\'' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
      | ' ' .. '~' -> 1
      | _ -> 4 end
  done;
  if !n = String.length str then str
  else begin
    let str' = Bytes.create !n in
    n := 0;
    for i = 0 to String.length str - 1 do
      begin match str.[i] with
      | ('\'' | '\\' as chr) ->
          Bytes.set str' !n '\\'; incr n;
          Bytes.set str' !n chr
      | '\n' ->
          Bytes.set str' !n '\\'; incr n;
          Bytes.set str' !n 'n'
      | '\t' ->
          Bytes.set str' !n '\\'; incr n;
          Bytes.set str' !n 't'
      | '\r' ->
          Bytes.set str' !n '\\'; incr n;
          Bytes.set str' !n 'r'
      | '\b' ->
          Bytes.set str' !n '\\'; incr n;
          Bytes.set str' !n 'b'
      | (' ' .. '~') as chr ->
          Bytes.set str' !n chr
      | chr ->
          let a = Char.code chr in
          Bytes.set str' !n '\\'; incr n;
          Bytes.set str' !n (Char.unsafe_chr (48 + a / 100)); incr n;
          Bytes.set str' !n (Char.unsafe_chr (48 + (a / 10) mod 10)); incr n;
          Bytes.set str' !n (Char.unsafe_chr (48 + a mod 10))
      end;
      incr n
    done;
    Bytes.unsafe_to_string str'
  end

let ocaml_escape str = String.escaped str

exception Lexical_error of string * string * int * int
}

let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule hurl_string buf = parse
  | '\'' { Buffer.contents buf }
  | '\\' ('\013'* '\010') [' ' '\000']*
    { hurl_string buf lexbuf }
  | '\\' (backslash_escapes as chr)
    { Buffer.add_char buf chr; hurl_string buf lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)
    { let v = decimal_code c d u in
      Buffer.add_char buf (Char.unsafe_chr v);
      hurl_string buf lexbuf }
  | '\\' 'o' (['0'-'3'] as c) (['0'-'7'] as d) (['0'-'7'] as u)
    { Buffer.add_char buf (char_for_octal_code c d u);
      hurl_string buf lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { Buffer.add_char buf (char_for_hexadecimal_code d u);
      hurl_string buf lexbuf }
  | '\\' 'u' '{' (['0'-'9' 'a'-'f' 'A'-'F'] + as s) '}'
    { let v = hexadecimal_code s in
      Buffer.add_utf_8_uchar buf (Uchar.unsafe_of_int v);
      hurl_string buf lexbuf }
  | '\\' (_ as chr)
    { Buffer.add_char buf '\\';
      Buffer.add_char buf chr;
      hurl_string buf lexbuf }
  | eof
    { raise (Lexical_error ("Unterminated string", "", 0, 0)) }
  | '\013'* '\010' as str 
    { Buffer.add_string buf str;
      hurl_string buf lexbuf }
  | _ as chr
    { Buffer.add_char buf chr;
      hurl_string buf lexbuf }

and ocaml_string buf = parse
  | '\'' { Buffer.contents buf }
  | '\\' ('\013'* '\010') [' ' '\000']*
    { ocaml_string buf lexbuf }
  | '\\' (backslash_escapes as chr)
    { Buffer.add_char buf chr; ocaml_string buf lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)
    { let v = decimal_code c d u in
      Buffer.add_char buf (Char.unsafe_chr v);
      ocaml_string buf lexbuf }
  | '\\' 'o' (['0'-'3'] as c) (['0'-'7'] as d) (['0'-'7'] as u)
    { Buffer.add_char buf (char_for_octal_code c d u);
      ocaml_string buf lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { Buffer.add_char buf (char_for_hexadecimal_code d u);
      ocaml_string buf lexbuf }
  | '\\' 'u' '{' (['0'-'9' 'a'-'f' 'A'-'F'] + as s) '}'
    { let v = hexadecimal_code s in
      Buffer.add_utf_8_uchar buf (Uchar.unsafe_of_int v);
      ocaml_string buf lexbuf }
  | '\\' (_ as chr)
    { Buffer.add_char buf '\\';
      Buffer.add_char buf chr;
      ocaml_string buf lexbuf }
  | eof
    { raise (Lexical_error ("Unterminated string", "", 0, 0)) }
  | '\013'* '\010' as str 
    { Buffer.add_string buf str;
      ocaml_string buf lexbuf }
  | _ as chr
    { Buffer.add_char buf chr;
      ocaml_string buf lexbuf }
