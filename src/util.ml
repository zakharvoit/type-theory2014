let parse_lambda s = Parser.only_expr Lexer.token (Lexing.from_string s)
let read_lambda c = Parser.only_expr Lexer.token (Lexing.from_channel c)
