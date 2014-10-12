# Example usage of the Parser. The test defines simple calculator
# language MATABC and demonstrates how programs written in that
# language are parsed. The test also customizes the final AST for
# easier interpretation.
#
# Copyright, 2014, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("..")

from pylangparser import *

# define all tokens in the language
IF = Keyword(r'if')

KEYWORDS = IF

PLUS = Operator(r'+')
MINUS = Operator(r'-')
ASSIGNMENT = Operator(r'=')
SEMICOLON = Operator(r';')
EQ = Operator(r'==')
LE = Operator(r'<')
GT = Operator(r'>')
LPAR = Operator(r'(')
RPAR = Operator(r')')

OPERATORS = PLUS & MINUS & ASSIGNMENT & EQ & LE & GT & SEMICOLON & LPAR & RPAR

IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

COMMENTS = Ignore(r'\#.*\n')

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')

CONSTANT = Symbols(r'[0-9]*')

TOKENS = KEYWORDS & OPERATORS & CONSTANT & IDENTIFIER & COMMENTS & IGNORE_CHARS

IgnoreTokensInAST(SEMICOLON & LPAR & RPAR)

# define our grammar

arthm_operator = \
    OperatorParser(PLUS) | \
    OperatorParser(MINUS)

comp_operator = \
    OperatorParser(LE) | \
    OperatorParser(GT) | \
    OperatorParser(EQ)

operand = \
    SymbolsParser(CONSTANT) | \
    SymbolsParser(IDENTIFIER)

def update_arthm_expression(result):
    token = result.get_token()

    if len(token) == 3:
       # p = 1
       # ('p', '=', '1')
       (lo, op, ro) = token
       token = (op, lo, ro)
    else:
       # p = 1 + (3 + 2)
       # ('p', '=', '1', ('3', '+', '2'))
       (lo1, op1, ro1, sub_token) = token
       (op2, ro2) = sub_token.get_token()
       sub_token.set_token((op2, ro1, ro2))
       token = (op1, lo1, sub_token)

    result.set_token(token)
    return result

arthm_expression = \
    CustomizeResult (SymbolsParser(IDENTIFIER) & \
    OperatorParser(ASSIGNMENT) & \
    operand & \
    Optional(arthm_operator & operand) & \
    OperatorParser(SEMICOLON), update_arthm_expression)

condition = \
    operand & \
    comp_operator & \
    operand

statement = RecursiveParser()

def update_condition(result):
    # p == 1
    # ('p', '==', '1')
    token = result.get_token()
    (lo, op, ro) = token
    result.set_token((op, lo, ro))
    return result

if_statement = \
    KeywordParser(IF) & \
    OperatorParser(LPAR) & \
    CustomizeResult (condition, update_condition) & \
    OperatorParser(RPAR) & \
    statement

# notice the usage of the '+=' operator below
statement += \
    if_statement | arthm_expression

program = AllTokensConsumed(ZeroOrMore(statement))

# our source code
source = """

# example program written in ABCMATH

p = 12;

if (p == 12)
  if (p == 5)
    p = 3 + 2;

"""

# obtain list of tokens present in the source
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(source)
print(tokens)

# build AST
result = program(tokens, 0)
result.pretty_print()
