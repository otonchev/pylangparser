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
CONSTANT = Symbols(r'[0-9]*')

C_STYLE_COMMENT = Ignore(r'\/\*.*(\*/){,1}')
CPP_STYLE_COMMENT = Ignore(r'\/\/[^\n]*')
MACROS = Ignore(r'\#.*\n')
IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

# group tokens into sub-groups
IGNORES = C_STYLE_COMMENT & CPP_STYLE_COMMENT & MACROS & IGNORE_CHARS

KEYWORDS = AUTO & BREAK & CASE & ENUM & CONST & CONTINUE & DEFAULT & DO & ELSE & \
    EXTERN & FOR & GOTO & IF & REGISTER & RETURN & SIZEOF & STATIC & STRUCT & \
    SWITCH & UNION & VOLATILE & WHILE & ENUM & TYPEDEF & VOID & CHAR & SHORT & \
    INT & LONG & FLOAT & DOUBLE & SIGNED & UNSIGNED

OPERATORS = DOT & COMMA & COLON & SEMICOLON & L_PAR & R_PAR & L_BRACKET & \
    R_BRACKET & L_BRACE & R_BRACE & STAR & DIV & MOD & AMPERSAND & PLUS & \
    MINUS & CARET & TILDE & EXCL_MARK & QUEST_MARK & BAR & ELLIPSIS & EQUAL & \
    EQ & NEQ & LT & LTEQ & GT & GTEQ & ARROW & PLUS_PLUS & MINUS_MINUS & \
    SHL & SHR & AMPERSAND_AMPERSAND & BAR_BAR & STAR_EQUAL & DIV_EQUAL & \
    MOD_EQUAL & PLUS_EQUAL & MINUS_EQUAL & SHL_EQUAL & SHR_EQUAL & \
    AMPERSAND_EQUAL & CARET_EQUAL & BAR_EQUAL

IDENTIFIERS = IDENTIFIER & CONSTANT & STRING_IDENTIFIER

# join all token sub-groups
TOKENS = KEYWORDS & OPERATORS & IDENTIFIERS & IGNORES


# our source code
source = r"""

#include <stdio.h>

struct struct_name {
  signed short int *p;
  char p;
  int * t;
  int p[5][5];
  /* comment */
};

"""

# obtain a list of all tokens present in the source
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(source)
print(tokens)

# define grammar

void_specifier = KeywordParser(VOID)

char_specifier = KeywordParser(CHAR)

signed_char_specifier = KeywordParser(SIGNED) & KeywordParser(CHAR)

# order is important as only the first series of parsers which apply
# will be considered
# That is why it is important to take "short int" before "short"
signed_short_specifier = \
    (KeywordParser(SHORT) & KeywordParser(INT)) | \
    (KeywordParser(SIGNED) & KeywordParser(SHORT) & KeywordParser(INT)) | \
    (KeywordParser(SIGNED) & KeywordParser(SHORT)) | \
    KeywordParser(SHORT)

signed_int_specifier = \
    (KeywordParser(SIGNED) & KeywordParser(INT)) | \
    KeywordParser(INT) | \
    KeywordParser(SIGNED)

signed_long_specifier = \
    (KeywordParser(SIGNED) & KeywordParser(LONG) & KeywordParser(INT)) | \
    (KeywordParser(SIGNED) & KeywordParser(LONG)) | \
    (KeywordParser(LONG) & KeywordParser(INT)) | \
    KeywordParser(LONG)

unsigned_char_specifier = KeywordParser(UNSIGNED) & KeywordParser(CHAR)

unsigned_short_specifier = \
    (KeywordParser(UNSIGNED) & KeywordParser(SHORT) & KeywordParser(INT)) | \
    (KeywordParser(UNSIGNED) & KeywordParser(SHORT))

unsigned_int_specifier = \
    (KeywordParser(UNSIGNED) & KeywordParser(INT)) | \
    KeywordParser(UNSIGNED)

unsigned_long_specifier = \
    (KeywordParser(UNSIGNED) & KeywordParser(LONG) & KeywordParser(INT)) | \
    (KeywordParser(UNSIGNED) & KeywordParser(LONG))

float_specifier = KeywordParser(FLOAT)

double_specifier = KeywordParser(DOUBLE)

long_double_specifier = KeywordParser(LONG) & KeywordParser(DOUBLE)

struct_specifier = KeywordParser(STRUCT) & SymbolsParser(IDENTIFIER)

union_specifier = KeywordParser(UNION) & SymbolsParser(IDENTIFIER)

enum_specifier = KeywordParser(ENUM) & SymbolsParser(IDENTIFIER)

type_specifier = void_specifier | char_specifier | signed_char_specifier | \
    signed_short_specifier | signed_int_specifier | signed_long_specifier | \
    unsigned_char_specifier | unsigned_short_specifier | unsigned_int_specifier | \
    unsigned_long_specifier | float_specifier | double_specifier | \
    long_double_specifier | struct_specifier | union_specifier | enum_specifier | \
    SymbolsParser(IDENTIFIER)

def get_abstract_pointer():
    return abstract_pointer
def get_abstract_array_declarator():
    return abstract_array_declarator
abstract_array_declarator = (OperatorParser(L_BRACKET) & \
    SymbolsParser(CONSTANT) & OperatorParser(R_BRACKET)) | \
    (OperatorParser(L_PAR) & RecursiveParser(get_abstract_pointer) & \
    OperatorParser(R_PAR) & OperatorParser(L_BRACKET) & SymbolsParser(CONSTANT) & \
    OperatorParser(R_BRACKET)) | (RecursiveParser(get_abstract_array_declarator) & \
    OperatorParser(L_BRACKET) & SymbolsParser(CONSTANT) & OperatorParser(R_BRACKET))

def get_parameter_list():
    return parameter_list
def get_abstract_pointer():
    return abstract_pointer
abstract_direct_declarator = abstract_array_declarator | \
    (OperatorParser(L_PAR) & RecursiveParser(get_abstract_pointer) & \
    OperatorParser(R_PAR) & OperatorParser(L_PAR) & \
    RecursiveParser(get_parameter_list) & OperatorParser(R_PAR))

def get_abstract_pointer():
    return abstract_pointer
abstract_pointer = (OperatorParser(STAR) & abstract_direct_declarator) | \
    (OperatorParser(STAR) & RecursiveParser(get_abstract_pointer))

abstract_declarator = abstract_pointer | abstract_direct_declarator

def get_declarator():
    return declarator
parameter_declaration = (type_specifier & RecursiveParser(get_declarator)) | \
    (type_specifier & abstract_declarator) | (SymbolsParser(IDENTIFIER) & \
    RecursiveParser(get_declarator)) | (SymbolsParser(IDENTIFIER) & \
    abstract_declarator)

parameter_list = parameter_declaration & Repeat(OperatorParser(COMMA) & \
    parameter_declaration)

def get_pointer():
    return pointer
array_declarator_tail = \
    OperatorParser(L_BRACKET) & Optional(SymbolsParser(CONSTANT)) & \
    OperatorParser(R_BRACKET)
array_declarator = (SymbolsParser(IDENTIFIER) & OperatorParser(L_BRACKET) & \
    Optional(SymbolsParser(CONSTANT)) & OperatorParser(R_BRACKET) & \
    ZeroOrMore(array_declarator_tail)) | \
    (OperatorParser(L_PAR) & RecursiveParser(get_pointer) & OperatorParser(R_PAR) & \
    OperatorParser(L_BRACKET) & Optional(SymbolsParser(CONSTANT)) & \
    OperatorParser(R_BRACKET) & ZeroOrMore(array_declarator_tail))

def get_function_pointer_declarator():
    return function_pointer_declarator
direct_declarator = array_declarator | SymbolsParser(IDENTIFIER) | \
    RecursiveParser(get_function_pointer_declarator)

pointer = (KeywordParser(STAR) & direct_declarator) | (KeywordParser(STAR) & \
    RecursiveParser(get_pointer))

function_pointer_declarator = OperatorParser(L_PAR) & pointer & \
    OperatorParser(R_PAR) & OperatorParser(L_PAR) & Optional(parameter_list) & \
    OperatorParser(R_PAR)

declarator = pointer | direct_declarator

member_declaration = type_specifier & declarator & OperatorParser(SEMICOLON)

struct_declaration = KeywordParser(STRUCT) & SymbolsParser(IDENTIFIER) & \
    OperatorParser(L_BRACE) & Repeat(member_declaration) & \
    OperatorParser(R_BRACE) & OperatorParser(SEMICOLON)

declaration_or_definition = struct_declaration #| union_declaration | \
    #enum_declaration |  typedef_declaration | function_declaration | \
    #variable_declaration | function_definition

translation_unit = Repeat(declaration_or_definition)

result = translation_unit(tokens, 0)
print(result)
