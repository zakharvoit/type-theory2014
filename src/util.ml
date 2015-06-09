let parse_lambda s = Parser.only_expr Lexer.token (Lexing.from_string s)
let read_lambda c = Parser.only_expr Lexer.token (Lexing.from_channel c)
let read_lambda_with_assign c = Parser.expr_with_assign Lexer.token
                                                        (Lexing.from_channel c)
