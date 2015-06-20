let (|>) f g x = f (g x)
let parse_string r s = r Lexer.token (Lexing.from_string s)
let parse_channel r c = r Lexer.token (Lexing.from_channel c)
