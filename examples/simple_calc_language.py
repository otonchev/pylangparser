# Example usage of the Parser. The test defines simple calculator
# language MATABC and demonstrates how programs written in that
# language are parsed.
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

# order is important as first operator that matches will be considered
# so it is important that '<=' is taken before '<'
OPERATORS = EQ & PLUS & MINUS & ASSIGNMENT & LE & GT & SEMICOLON & LPAR & RPAR

IGNORE_CHARS = Ignore(r'[ \t\v\f\n]+')

COMMENTS = Ignore(r'\#.*\n')

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')

CONSTANT = Symbols(r'[0-9]+')

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

arthm_expression = \
    SymbolsParser(IDENTIFIER) & \
    OperatorParser(ASSIGNMENT) & \
    (operand << Optional(arthm_operator << operand)) & \
    OperatorParser(SEMICOLON)

condition = \
    operand << \
    comp_operator << \
    operand

statement = RecursiveParser()

if_statement = \
    KeywordParser(IF) & \
    OperatorParser(LPAR) & \
    condition & \
    OperatorParser(RPAR) & \
    statement

# not the usage of the '+=' operator below
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
