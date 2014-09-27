# Example usage of the Parser. The test parses a C program
# and validates the grammar. The simplified C grammer used in the example
# is taken from the SableCC project. See c_parser_grammar.txt for more
# details.
#
# Copyright, 2014, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("..")

from pylangparser import *

# define all tokens in the language
AUTO = Keyword(r'auto')
BREAK = Keyword(r'break')
CASE = Keyword(r'case')
CONST = Keyword(r'const')
CONTINUE = Keyword(r'continue')
DEFAULT = Keyword(r'default')
DO = Keyword(r'do')
ELSE = Keyword(r'else')
EXTERN = Keyword(r'extern')
FOR = Keyword(r'for')
GOTO = Keyword(r'goto')
IF = Keyword(r'if')
REGISTER = Keyword(r'register')
RETURN = Keyword(r'return')
SIZEOF = Keyword(r'sizeof')
STATIC = Keyword(r'static')
STRUCT = Keyword(r'struct')
SWITCH = Keyword(r'switch')
UNION = Keyword(r'union')
VOLATILE = Keyword(r'volatile')
WHILE = Keyword(r'while')
ENUM = Keyword(r'enum')
TYPEDEF = Keyword(r'typedef')
VOID = Keyword(r'void')
CHAR = Keyword(r'char')
SHORT = Keyword(r'short')
INT = Keyword(r'int')
LONG = Keyword(r'long')
FLOAT = Keyword(r'float')
DOUBLE = Keyword(r'double')
SIGNED = Keyword(r'signed')
UNSIGNED = Keyword(r'unsigned')

DOT = Operator(r'.')
COMMA = Operator(r',')
COLON = Operator(r':')
SEMICOLON = Operator(r';')
L_PAR = Operator(r'(')
R_PAR = Operator(r')')
L_BRACKET = Operator(r'[')
R_BRACKET = Operator(r']')
L_BRACE = Operator(r'{')
R_BRACE = Operator(r'}')
STAR = Operator(r'*')
DIV = Operator(r'/')
MOD = Operator(r'%')
AMPERSAND = Operator(r'&')
PLUS = Operator(r'+')
MINUS = Operator(r'-')
CARET = Operator(r'^')
TILDE = Operator(r'~')
EXCL_MARK = Operator(r'!')
QUEST_MARK = Operator(r'?')
BAR = Operator(r'|')
ELLIPSIS = Operator(r'...')
EQUAL = Operator(r'=')
EQ = Operator(r'==')
NEQ = Operator(r'!=')
LT = Operator(r'<')
LTEQ = Operator(r'<=')
GT = Operator(r'>')
GTEQ = Operator(r'>=')
ARROW = Operator(r'->')
PLUS_PLUS = Operator(r'++')
MINUS_MINUS = Operator(r'--')
SHL = Operator(r'<<')
SHR = Operator(r'>>')
AMPERSAND_AMPERSAND = Operator(r'&&')
BAR_BAR = Operator(r'||')
STAR_EQUAL = Operator(r'*=')
DIV_EQUAL = Operator(r'/=')
MOD_EQUAL = Operator(r'%=')
PLUS_EQUAL = Operator(r'+=')
MINUS_EQUAL = Operator(r'-=')
SHL_EQUAL = Operator(r'<<=')
SHR_EQUAL = Operator(r'>>=')
AMPERSAND_EQUAL = Operator(r'&=')
CARET_EQUAL = Operator(r'^=')
BAR_EQUAL = Operator(r'|=')

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
STRING_IDENTIFIER = Symbols(r'\".*\"')
CONSTANT = Symbols(r'[0-9]*') #FIXME

C_STYLE_COMMENT = Ignore(r'/\*.*\*/')
CPP_STYLE_COMMENT = Ignore(r'//.*\n')
MACROS = Ignore(r'#.*\n')
IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

# group tokens into sub-groups
IGNORES = C_STYLE_COMMENT & CPP_STYLE_COMMENT & MACROS & IGNORE_CHARS

KEYWORDS = AUTO & BREAK & CASE & ENUM & CONST & CONTINUE & DEFAULT & DO & ELSE & \
    EXTERN & FOR & GOTO & IF & REGISTER & RETURN & SIZEOF & STATIC & STRUCT & \
    SWITCH & UNION & VOLATILE & WHILE & ENUM & TYPEDEF & VOID & CHAR & SHORT & \
    INT & LONG & FLOAT & DOUBLE & SIGNED & UNSIGNED

OperatorS = DOT & COMMA & COLON & SEMICOLON & L_PAR & R_PAR & L_BRACKET & \
    R_BRACKET & L_BRACE & R_BRACE & STAR & DIV & MOD & AMPERSAND & PLUS & \
    MINUS & CARET & TILDE & EXCL_MARK & QUEST_MARK & BAR & ELLIPSIS & EQUAL & \
    EQ & NEQ & LT & LTEQ & GT & GTEQ & ARROW & PLUS_PLUS & MINUS_MINUS & \
    SHL & SHR & AMPERSAND_AMPERSAND & BAR_BAR & STAR_EQUAL & DIV_EQUAL & \
    MOD_EQUAL & PLUS_EQUAL & MINUS_EQUAL & SHL_EQUAL & SHR_EQUAL & \
    AMPERSAND_EQUAL & CARET_EQUAL & BAR_EQUAL

IDENTIFIERS = IDENTIFIER & CONSTANT & STRING_IDENTIFIER

# join all token sub-groups
TOKENS = KEYWORDS & OperatorS & IDENTIFIERS & IGNORES


# our source code
source = r"""

p--p++p()

"""

# obtain a list of all tokens present in the source
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(source)
print(tokens)

# define grammar

#jump_statement = (KeywordParser(GOTO) & SymbolsParser(IDENTIFIER) & \
#    OperatorParser(COLON)) | (KeywordParser(CONTINUE) & \
#    OperatorParser(COLON)) | (KeywordParser(BREAK) & \
#    OperatorParser(COLON)) | (KeywordParser(RETURN) & \
#    OperatorParser(COLON)) | (KeywordParser(RETURN) & \
#    expression)

#result = jump_statement(tokens, 0)

#result = postfix_expression(tokens, 0)
#print(result)
