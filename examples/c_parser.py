# Example usage of the Parser. The test parses a C program
# and validates the grammar. The simplified C grammar used in the example
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
INT_CONSTANT = Symbols(r'(0x){0,1}[0-9]+')
FLOAT_CONSTANT = Symbols(r'[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?')
CHAR_CONSTANT = Symbols(r'\'.\'')

CPP_STYLE_COMMENT = Ignore(r'\/\/[^\n]*')
MACROS = Ignore(r'\#.*\n')
IGNORE_CHARS = Ignore(r'[ \t\v\f]+')
CONST = Ignore(r'const')

# group tokens into sub-groups

# Note: order is important as the first entry in the list that matches a given
# token will be considered. Example:
#
# CONST = Ignore(r'const')
# IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
#
# if IDENTIFIER appears before CONST, the latter will never appear in the
# result list as 'const' matches both tokens

IGNORES = CONST & CPP_STYLE_COMMENT & MACROS & IGNORE_CHARS

KEYWORDS = AUTO & BREAK & CASE & ENUM & CONTINUE & DEFAULT & DO & ELSE & \
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

IDENTIFIERS = IDENTIFIER & STRING_IDENTIFIER & INT_CONSTANT & FLOAT_CONSTANT

# join all token sub-groups
TOKENS = IGNORES & KEYWORDS & OPERATORS & IDENTIFIERS

#
# list tokens to be IGNORED in the final AST
#
IgnoreTokensInAST(SEMICOLON & L_BRACE & R_BRACE & R_PAR & L_PAR & L_BRACKET & \
    R_BRACKET & COMMA & COLON)


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

union union_name {
  signed short int *p;
  char p;
  int * t;
  int p[5][5];
  /* comment */
};

typedef unsigned char BYTE;

unsigned int p, c;

enum some_name{p = 1, q};

char * func(int p, char t);

int
func2(const int p, char t) {
  int l, q;
  char *f;
  unsigned short j;

  q = 5.5;
  func(12, q, 42);

  {
    {
      q = 1;
    }
    q = 5;
    return f;
  }

  if (5 == 6) {
  } else {
    p = 1;
  }

  while (5 == 6) {
    p = 1;
  }

  do {
    p = 1;
  } while (5 == 6);

  for (;;) {
    p = 1;
    break;
  }

  for (i = 5; i < 5; i++) {
    p = 1;
    if (i == 4) {
      break;
    }
  }

  switch (i) {
    case 5: {
      break;
    }
    default: {
      break;
    }
  }

  switch (i) {
    case 5:
      break;
    default:
      break;
  }

  if (p == 5)
    p = 5;
  else
    goto error;

  /*
   * this is a multi-line comment
   */

  return (p == 5);

error: {
  if (p == 5)
    p = 5;
  return 1;
}
}

"""

# obtain a list of all tokens present in the source
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(source)
print("-------\nTOKENS:\n-------")
print(tokens)

#
# define C grammar
#
unop = \
    OperatorParser(PLUS) | \
    OperatorParser(MINUS) | \
    OperatorParser(TILDE) | \
    OperatorParser(EXCL_MARK)

relop = \
    OperatorParser(EQ) | \
    OperatorParser(NEQ) | \
    OperatorParser(LT) | \
    OperatorParser(LTEQ) | \
    OperatorParser(GT) | \
    OperatorParser(GTEQ)

binop = \
    relop | \
    OperatorParser(STAR) | \
    OperatorParser(DIV) | \
    OperatorParser(MOD) | \
    OperatorParser(AMPERSAND) | \
    OperatorParser(PLUS) | \
    OperatorParser(MINUS) | \
    OperatorParser(CARET) | \
    OperatorParser(EXCL_MARK) | \
    OperatorParser(BAR) | \
    OperatorParser(SHL) | \
    OperatorParser(SHR) | \
    OperatorParser(AMPERSAND_AMPERSAND) | \
    OperatorParser(BAR_BAR)

prefop = \
    OperatorParser(PLUS_PLUS) | \
    OperatorParser(MINUS_MINUS)

postfop = \
    OperatorParser(PLUS_PLUS) | \
    OperatorParser(MINUS_MINUS)

void_specifier = KeywordParser(VOID)

char_specifier = KeywordParser(CHAR)

signed_char_specifier = KeywordParser(SIGNED) & KeywordParser(CHAR)

# Note: order is important as only the first sub-group of parsers
# (parser1 & parser2 &...& parsern) which applies will be considered
# That's why it is important to take "short int" before "short"
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

type_specifier = \
    void_specifier | \
    char_specifier | \
    signed_char_specifier | \
    signed_short_specifier | \
    signed_int_specifier | \
    signed_long_specifier | \
    unsigned_char_specifier | \
    unsigned_short_specifier | \
    unsigned_int_specifier | \
    unsigned_long_specifier | \
    float_specifier | \
    double_specifier | \
    long_double_specifier | \
    struct_specifier | \
    union_specifier | \
    enum_specifier | \
    SymbolsParser(IDENTIFIER)

def get_abstract_pointer():
    return abstract_pointer

abstract_array_declarator_tail = \
    OperatorParser(L_BRACKET) & \
    Optional(SymbolsParser(INT_CONSTANT)) & \
    OperatorParser(R_BRACKET)

abstract_array_declarator = \
    (OperatorParser(L_BRACKET) & \
        Optional(SymbolsParser(INT_CONSTANT)) & OperatorParser(R_BRACKET) & \
        Optional(abstract_array_declarator_tail)) | \
    (OperatorParser(L_PAR) & RecursiveParser(get_abstract_pointer) & \
        OperatorParser(R_PAR) & OperatorParser(L_BRACKET) & \
        Optional(SymbolsParser(INT_CONSTANT)) & OperatorParser(R_BRACKET) & \
        Optional(abstract_array_declarator_tail))

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
abstract_pointer = \
    (OperatorParser(STAR) & Optional(abstract_direct_declarator)) | \
    (OperatorParser(STAR) & RecursiveParser(get_abstract_pointer))

abstract_declarator = abstract_pointer | abstract_direct_declarator

def get_declarator():
    return declarator
parameter_declaration = (type_specifier & RecursiveParser(get_declarator)) | \
    (type_specifier & abstract_declarator) | (SymbolsParser(IDENTIFIER) & \
    RecursiveParser(get_declarator)) | (SymbolsParser(IDENTIFIER) & \
    abstract_declarator)

parameter_list = parameter_declaration & \
    Optional(Repeat(OperatorParser(COMMA) & parameter_declaration))

def get_pointer():
    return pointer
array_declarator_tail = \
    OperatorParser(L_BRACKET) & Optional(SymbolsParser(INT_CONSTANT)) & \
    OperatorParser(R_BRACKET)
array_declarator = (SymbolsParser(IDENTIFIER) & OperatorParser(L_BRACKET) & \
    Optional(SymbolsParser(INT_CONSTANT)) & OperatorParser(R_BRACKET) & \
        ZeroOrMore(array_declarator_tail)) | \
    (OperatorParser(L_PAR) & RecursiveParser(get_pointer) & \
        OperatorParser(R_PAR) & OperatorParser(L_BRACKET) & \
        Optional(SymbolsParser(INT_CONSTANT)) & OperatorParser(R_BRACKET) & \
        ZeroOrMore(array_declarator_tail))

def get_function_pointer_declarator():
    return function_pointer_declarator
direct_declarator = array_declarator | SymbolsParser(IDENTIFIER) | \
    RecursiveParser(get_function_pointer_declarator)

pointer = (OperatorParser(STAR) & direct_declarator) | (OperatorParser(STAR) & \
    RecursiveParser(get_pointer))

function_pointer_declarator = OperatorParser(L_PAR) & pointer & \
    OperatorParser(R_PAR) & OperatorParser(L_PAR) & Optional(parameter_list) & \
    OperatorParser(R_PAR)

declarator = pointer | direct_declarator

def get_rhs():
    return rhs
declarator_with_modifier = \
    (pointer | direct_declarator) & \
    Optional(OperatorParser(EQUAL) & \
    RecursiveParser(get_rhs))

member_declaration = type_specifier & declarator & OperatorParser(SEMICOLON)

# main groups:
#              struct_declaration,
#              union_declaration,
#              enum_declaration,
#              typedef_declaration,
#              function_declaration,
#              variable_declaration,
#              function_definition
#

#
# struct declaration
#
struct_declaration = \
    KeywordParser(STRUCT) & SymbolsParser(IDENTIFIER) & \
    OperatorParser(L_BRACE) & Repeat(member_declaration) & \
    OperatorParser(R_BRACE) & OperatorParser(SEMICOLON)

#
# union declaration
#
union_declaration = \
    KeywordParser(UNION) & SymbolsParser(IDENTIFIER) & \
    OperatorParser(L_BRACE) & Repeat(member_declaration) & \
    OperatorParser(R_BRACE) & OperatorParser(SEMICOLON)

#
# typedef declaration
#
typedef_declaration = \
    (KeywordParser(TYPEDEF) & type_specifier & declarator & \
    OperatorParser(SEMICOLON)) | \
    (KeywordParser(TYPEDEF) & SymbolsParser(IDENTIFIER) & \
    declarator & OperatorParser(SEMICOLON));

additional_declarator_with_modifier = \
        OperatorParser(COMMA) & declarator_with_modifier

#
# variable declaration
#
variable_declaration = \
    (type_specifier & declarator_with_modifier & \
        ZeroOrMore(additional_declarator_with_modifier) & \
        OperatorParser(SEMICOLON)) | \
    (SymbolsParser(IDENTIFIER) & declarator_with_modifier & \
        ZeroOrMore(additional_declarator_with_modifier) & \
        OperatorParser(SEMICOLON))

constant = \
    (Optional(unop) & SymbolsParser(FLOAT_CONSTANT)) | \
    (Optional(unop) & SymbolsParser(IDENTIFIER)) | \
    (Optional(unop) & SymbolsParser(INT_CONSTANT)) | \
    (Optional(unop) & SymbolsParser(CHAR_CONSTANT))

enumerator = \
    (SymbolsParser(IDENTIFIER) & OperatorParser(EQUAL) & \
        constant) | \
    SymbolsParser(IDENTIFIER)
#
#  enum declaration
#
enum_declaration = \
    KeywordParser(ENUM) & SymbolsParser(IDENTIFIER) & \
    OperatorParser(L_BRACE) & enumerator & ZeroOrMore(OperatorParser(COMMA) & \
    enumerator) & OperatorParser(R_BRACE) & OperatorParser(SEMICOLON)


#
# function declaration
#

def get_pointer_function():
    return pointer_function

array_function_declarator = \
    (OperatorParser(L_PAR) & RecursiveParser(get_pointer_function) & \
    OperatorParser(R_PAR) & Repeat(OperatorParser(L_BRACKET) & \
    SymbolsParser(INT_CONSTANT) & OperatorParser(R_BRACKET)))

direct_function_declarator = \
    array_function_declarator | \
    (SymbolsParser(IDENTIFIER) & OperatorParser(L_PAR) & \
        ZeroOrMore(parameter_list) & OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & RecursiveParser(get_pointer_function) & \
        OperatorParser(R_PAR) & OperatorParser(L_PAR) & \
        ZeroOrMore(parameter_list) & OperatorParser(R_PAR))

pointer_function = \
    (OperatorParser(STAR) & direct_function_declarator) | \
    (OperatorParser(STAR) & RecursiveParser(get_pointer_function))

function_declarator = \
    direct_function_declarator | pointer_function

function_declaration = \
    (type_specifier & function_declarator & OperatorParser(SEMICOLON)) | \
    (SymbolsParser(IDENTIFIER) & function_declarator & \
        OperatorParser(SEMICOLON))

#
# function definition
#

idlist = \
    OperatorParser(DOT) & SymbolsParser(IDENTIFIER)

compref = \
    OperatorParser(L_PAR) & OperatorParser(STAR) & SymbolsParser(IDENTIFIER) & \
        OperatorParser(R_PAR) & Repeat(idlist) | \
    SymbolsParser(IDENTIFIER) & Repeat(idlist)

value = SymbolsParser(IDENTIFIER) | constant

reflist = OperatorParser(L_BRACKET) & value & OperatorParser(R_BRACKET)

arrayref = SymbolsParser(IDENTIFIER) & Repeat(reflist)

varname = arrayref | compref | SymbolsParser(IDENTIFIER)

type_name = \
    type_specifier & Optional(abstract_declarator) | \
    SymbolsParser(IDENTIFIER) & Optional(abstract_declarator)

conditional_expression = \
    (value & relop & value) | \
    value

simple_expression = \
    varname | \
    constant

binary_expression = \
    (OperatorParser(L_PAR) & SymbolsParser(IDENTIFIER) & binop & value & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & constant & binop & value & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & value & binop & value & OperatorParser(R_PAR))

arglist = \
    value & ZeroOrMore(OperatorParser(COMMA) & value)

call_expression = \
    SymbolsParser(IDENTIFIER) & OperatorParser(L_PAR) & Optional(arglist) & \
    OperatorParser(R_PAR)

unary_expression = \
    simple_expression | \
    (OperatorParser(L_PAR) & OperatorParser(STAR) & \
        SymbolsParser(IDENTIFIER) & OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & OperatorParser(AMPERSAND) & varname & \
        OperatorParser(R_PAR)) | \
    call_expression | \
    (unop & SymbolsParser(IDENTIFIER)) | \
    (OperatorParser(L_PAR) & unop & SymbolsParser(IDENTIFIER) & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & varname) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & constant)

rhs = \
    call_expression | \
    binary_expression | \
    unary_expression

modify_expression = \
    (varname & OperatorParser(EQUAL) & rhs) | \
    (varname & postfop) | \
    (prefop & varname) | \
    (OperatorParser(L_PAR) & OperatorParser(STAR) & \
        SymbolsParser(IDENTIFIER) & OperatorParser(R_PAR) & \
        OperatorParser(EQUAL) & rhs)

basic_statement = \
    call_expression | \
    modify_expression | \
    simple_expression | \
    (OperatorParser(L_PAR) & OperatorParser(STAR) & \
        SymbolsParser(IDENTIFIER) & OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & OperatorParser(AMPERSAND) & varname & \
        OperatorParser(R_PAR)) | \
    (unop & SymbolsParser(IDENTIFIER)) | \
    (OperatorParser(L_PAR) & unop & SymbolsParser(IDENTIFIER) & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & \
        varname) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & \
        constant)

def get_statement():
    return statement
def get_stop_statement():
    return stop_statement
compound_statement = \
    OperatorParser(L_BRACE) & ZeroOrMore(RecursiveParser(get_statement)) & \
    Optional(RecursiveParser(get_stop_statement)) & OperatorParser(R_BRACE)

default_statement = \
    (KeywordParser(DEFAULT) & OperatorParser(COLON) & \
        OperatorParser(L_BRACE) & ZeroOrMore(RecursiveParser(get_statement)) & \
        Optional(RecursiveParser(get_stop_statement)) & \
        OperatorParser(R_BRACE)) | \
    (KeywordParser(DEFAULT) & OperatorParser(COLON) & \
        ZeroOrMore(RecursiveParser(get_statement)) & \
        Optional(RecursiveParser(get_stop_statement))) | \
    (KeywordParser(DEFAULT) & OperatorParser(COLON))

case_statement = \
    (KeywordParser(CASE) & constant & OperatorParser(COLON) & \
        OperatorParser(L_BRACE) & ZeroOrMore(RecursiveParser(get_statement)) & \
        Optional(RecursiveParser(get_stop_statement)) & \
        OperatorParser(R_BRACE)) | \
    (KeywordParser(CASE) & constant & OperatorParser(COLON) & \
        ZeroOrMore(RecursiveParser(get_statement)) & \
        Optional(RecursiveParser(get_stop_statement))) | \
    (KeywordParser(CASE) & constant & OperatorParser(COLON))

case_statements = \
    OperatorParser(L_BRACE) & Repeat(case_statement) & \
        Optional(default_statement) & OperatorParser(R_BRACE)

goto_statement = \
    KeywordParser(GOTO) & SymbolsParser(IDENTIFIER) & OperatorParser(SEMICOLON)

statement = \
    goto_statement | \
    compound_statement | \
    (basic_statement & OperatorParser(SEMICOLON)) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON) & \
        KeywordParser(ELSE) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON) & \
        KeywordParser(ELSE) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & RecursiveParser(get_statement) & \
        KeywordParser(ELSE) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement & KeywordParser(ELSE) & \
        compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement & KeywordParser(ELSE) & \
        RecursiveParser(get_statement)) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & RecursiveParser(get_statement) & \
        KeywordParser(ELSE) & RecursiveParser(get_statement)) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & RecursiveParser(get_statement)) | \
    (KeywordParser(WHILE) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement) | \
    (KeywordParser(DO) & compound_statement & KeywordParser(WHILE) & \
        OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(FOR) & OperatorParser(L_PAR) & Optional(basic_statement) & \
        OperatorParser(SEMICOLON) & Optional(conditional_expression) & \
        OperatorParser(SEMICOLON) & Optional(basic_statement) & \
        OperatorParser(R_PAR) & compound_statement) | \
    (KeywordParser(SWITCH) & OperatorParser(L_PAR) & value & \
        OperatorParser(R_PAR) & case_statements)

dead_code = \
    (KeywordParser(BREAK) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(CONTINUE) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(RETURN) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(RETURN) & value & OperatorParser(SEMICOLON)) | \
    (KeywordParser(RETURN) & OperatorParser(L_PAR) & value & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON))

stop_statement = \
    (KeywordParser(RETURN) & statement & ZeroOrMore(dead_code)) | \
    (KeywordParser(BREAK) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(CONTINUE) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & binary_expression & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & value & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & OperatorParser(L_PAR) & value & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (goto_statement & ZeroOrMore(dead_code))

labeled_statement = \
    (SymbolsParser(IDENTIFIER) & OperatorParser(COLON) & \
        OperatorParser(L_BRACE) & ZeroOrMore(statement) & \
        ZeroOrMore(stop_statement) & OperatorParser(R_BRACE)) | \
    (SymbolsParser(IDENTIFIER) & OperatorParser(COLON) & \
        ZeroOrMore(statement) & ZeroOrMore(stop_statement))

function_body = \
    OperatorParser(L_BRACE) & ZeroOrMore(variable_declaration) & \
    ZeroOrMore(statement) & ZeroOrMore(stop_statement) & \
    ZeroOrMore(labeled_statement) & OperatorParser(R_BRACE)

function_definition = \
    (type_specifier & function_declarator & function_body) | \
    (SymbolsParser(IDENTIFIER) & function_declarator & function_body)

declaration_or_definition = \
    struct_declaration | \
    union_declaration | \
    typedef_declaration | \
    enum_declaration | \
    variable_declaration | \
    function_declaration | \
    function_definition

#
# translation unit will parse the whole program
#
translation_unit = \
    CheckErrors(AllTokensConsumed(Repeat(declaration_or_definition)))

result = translation_unit(tokens, 0)
print("----\nAST:\n----")
print(result)
print("--------------\nHumanized AST:\n--------------")
result.pretty_print()

print("\n------subgroup 1------")
group = result.get_sub_group(1)
group.pretty_print()

print("\n------subgroup 2------")
group = result.get_sub_group(2)
group.pretty_print()

print("\n------subgroup 3------")
group = result.get_sub_group(3)
group.pretty_print()

print("\n------subgroup 4------")
group = result.get_sub_group(4)
group.pretty_print()

print("\n------subgroup 5------")
group = result.get_sub_group(5)
group.pretty_print()

print("\n------subgroup 6------")
group = result.get_sub_group(6)
group.pretty_print()

print("\n------subgroup 7------")
group = result.get_sub_group(7)
group.pretty_print()

print("\n------subgroup 7.1------")
subgroup = group.get_sub_group(1)
subgroup.pretty_print()

print("\n------subgroup 7.2------")
subgroup = group.get_sub_group(2)
subgroup.pretty_print()

print("\n------subgroup 7.3------")
subgroup = group.get_sub_group(3)
subgroup.pretty_print()

#print all function declarations
print("\n--------------function declarations--------------")
index = 1
group = result.get_sub_group(index)
while group:
    if group.is_instance(function_declaration):
        group.pretty_print()
    group = result.get_sub_group(index)
    index = index + 1
