#ifndef TOKEN
#define TOKEN(name)
#endif

#ifndef TOKEN_KEYWORD
#define TOKEN_KEYWORD(name, sourcerepr) TOKEN(kw_ ## name)
#endif

#ifndef TOKEN_SYMBOL
#define TOKEN_SYMBOL(name, character) TOKEN(sym_ ## name)
#endif

TOKEN(identifier)
TOKEN(integer_literal)
TOKEN(decimal_literal)
TOKEN(string_literal)
TOKEN(char_literal)

TOKEN(eof)
TOKEN(whitespace)
TOKEN(newline)
TOKEN(comment_single_line)
TOKEN(comment_multiple_line)

TOKEN_KEYWORD(def, def)
TOKEN_KEYWORD(var, var)
TOKEN_KEYWORD(extern, extern)
TOKEN_KEYWORD(return, return)
TOKEN_KEYWORD(true, true)
TOKEN_KEYWORD(false, false)
TOKEN_KEYWORD(if, if)
TOKEN_KEYWORD(else, else)
TOKEN_KEYWORD(while, while)
TOKEN_KEYWORD(as, as)
TOKEN_KEYWORD(struct, struct)
TOKEN_KEYWORD(do, do)

TOKEN_SYMBOL(colon, ":")
TOKEN_SYMBOL(comma, ",")
TOKEN_SYMBOL(left_paren, "(")
TOKEN_SYMBOL(right_paren, ")")
TOKEN_SYMBOL(left_curly, "{")
TOKEN_SYMBOL(right_curly, "}")
TOKEN_SYMBOL(dot, ".")

TOKEN_SYMBOL(add_assignment, "+=")
TOKEN_SYMBOL(sub_assignment, "-=")
TOKEN_SYMBOL(mult_assignment, "*=")
TOKEN_SYMBOL(div_assignment, "/=")
TOKEN_SYMBOL(mod_assignment, "%=")
TOKEN_SYMBOL(or_assignment, "|=")
TOKEN_SYMBOL(and_assignment, "&=")
TOKEN_SYMBOL(add, "+")
TOKEN_SYMBOL(subtract, "-")
TOKEN_SYMBOL(asterisk, "*")
TOKEN_SYMBOL(divide, "/")
TOKEN_SYMBOL(modulo, "%")
TOKEN_SYMBOL(equal, "==")
TOKEN_SYMBOL(not_equal, "!=")
TOKEN_SYMBOL(less_than_or_equal, "<=")
TOKEN_SYMBOL(less_than, "<")
TOKEN_SYMBOL(greater_than_or_equal, ">=")
TOKEN_SYMBOL(greater_than, ">")
TOKEN_SYMBOL(b_or, "||")
TOKEN_SYMBOL(b_and, "&&")
TOKEN_SYMBOL(b_not, "!")
TOKEN_SYMBOL(assignment, "=")
TOKEN_SYMBOL(ampersand, "&")
TOKEN_SYMBOL(pipe, "|")

#undef TOKEN
#undef TOKEN_KEYWORD
#undef TOKEN_SYMBOL