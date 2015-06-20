let (|>) f g x = f (g x)
let parse_string r s = r Lexer.token (Lexing.from_string s)
let parse_channel r c = r Lexer.token (Lexing.from_channel c)

let parse_lambda s = Lambda_parser.only_expr Lambda_lexer.token (Lexing.from_string s)
let read_lambda c = Lambda_parser.only_expr Lambda_lexer.token (Lexing.from_channel c)
let read_lambda_with_assign c = Lambda_parser.expr_with_assign Lambda_lexer.token
                                                        (Lexing.from_channel c)
