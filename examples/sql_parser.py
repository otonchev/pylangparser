# Example usage of the Parser. The test parses SQL scrips
# and validates the grammar.
# Supported SQL grammar:
#     http://www.stowe8miler.org/sqlgrmr.html
#
# Copyright, 2014, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("..")

from pylangparser import *

SELECT = Keyword(r'SELECT', ignorecase=True)
INSERT = Keyword(r'INSERT', ignorecase=True)
DELETE = Keyword(r'DELETE', ignorecase=True)
UPDATE = Keyword(r'UPDATE', ignorecase=True)
FROM = Keyword(r'FROM', ignorecase=True)
INTO = Keyword(r'INTO', ignorecase=True)
SET = Keyword(r'SET', ignorecase=True)
WHERE = Keyword(r'WHERE', ignorecase=True)
HAVING = Keyword(r'HAVING', ignorecase=True)
DISTINCT = Keyword(r'DISTINCT', ignorecase=True)
ALL = Keyword(r'ALL', ignorecase=True)
ASC = Keyword(r'ASC', ignorecase=True)
DESC = Keyword(r'DESC', ignorecase=True)
GROUP_BY = Keyword(r'GROUP BY', ignorecase=True)
ORDER_BY = Keyword(r'ORDER BY', ignorecase=True)
COUNT = Keyword(r'COUNT', ignorecase=True)
AVG = Keyword(r'AVG', ignorecase=True)
MAX = Keyword(r'MAX', ignorecase=True)
MIN = Keyword(r'MIN', ignorecase=True)
SUM = Keyword(r'SUM', ignorecase=True)
VALUES = Keyword(r'VALUES', ignorecase=True)
NULL = Keyword(r'NULL', ignorecase=True)
IN = Keyword(r'IN', ignorecase=True)
IS = Keyword(r'IS', ignorecase=True)
NOT = Keyword(r'NOT', ignorecase=True)
LIKE = Keyword(r'LIKE', ignorecase=True)
AND = Keyword(r'AND', ignorecase=True)
OR = Keyword(r'OR', ignorecase=True)

KEYWORDS = SELECT & INSERT & DELETE & UPDATE & FROM & INTO & SET & WHERE & \
    HAVING & DISTINCT & ALL & ASC & DESC & GROUP_BY & ORDER_BY & COUNT & \
    AVG & MAX & MIN & SUM & VALUES & NULL & IN & IS & NOT & LIKE & AND & OR

IDENTIFIER = Symbols(r'[a-zA-Z_]+')
STRING = Symbols(r'\'[a-zA-Z_]+\'')
REALNUMBER = Symbols(r'\d+\.\d*')
INTEGER = Symbols(r'[0-9]+')
DATE = Symbols(r'd\'[0-9]{4}-[0-9]{2}-[0-9]{2}\'')
TIME = Symbols(r't\'[0-9]{2}:[0-9]{2}:[0-9]{2}\'')
TIMESTAMP = Symbols(r'ts\'[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}\'')

SYMBOLS = IDENTIFIER & STRING & REALNUMBER & INTEGER & DATE & TIME & TIMESTAMP

SEMICOLON = Operator(r';')
COMMA = Operator(r',')
STAR = Operator(r'*')
L_PAR = Operator(r'(')
R_PAR = Operator(r')')
PLUS = Operator(r'+')
MINUS = Operator(r'-')
DIV = Operator(r'/')
ASSIGNMENT = Operator(r'=')
GT = Operator(r'>')
GE = Operator(r'>=')
LE = Operator(r'<')
LQ = Operator(r'<=')
DIFFERENT = Operator(r'<>')

OPERATORS = SEMICOLON & COMMA & STAR & L_PAR & R_PAR & PLUS & MINUS & DIV & \
    ASSIGNMENT & GT & GE & LE & LQ & DIFFERENT

IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

TOKENS = KEYWORDS & SYMBOLS & OPERATORS & IGNORE_CHARS

IgnoreTokensInAST(SEMICOLON & R_PAR & L_PAR & COMMA)

sql = """
    SELECT CustomerName,City FROM Customers;
    SELECT CustomerName,City FROM Customers WHERE CustomerID=1.5;
    select CustomerName,City from Customers;
"""

tablename = SymbolsParser(IDENTIFIER)
columnname = SymbolsParser(IDENTIFIER)

table = tablename

aliasname = SymbolsParser(IDENTIFIER)

tableref = table | (table & aliasname)

def get_tablelist():
    return tablelist
tablelist = \
    (tableref & OperatorParser(COMMA) & RecursiveParser(get_tablelist)) | \
    tableref

colref = (aliasname & OperatorParser(COMMA) & columnname) | columnname

asc = Optional(KeywordParser(ASC) | KeywordParser(DESC))

orderbyterm = (colref & asc) | (SymbolsParser(INTEGER) & asc)

def get_orderbyterms():
    return orderbyterms
orderbyterms = orderbyterm | \
    (orderbyterm & OperatorParser(COMMA) & RecursiveParser(get_orderbyterms))

orderby = Optional(KeywordParser(ORDER_BY) & orderbyterms)

def get_groupbyterms():
    return groupbyterms
groupbyterms = \
    colref | \
    (colref & OperatorParser(COMMA) & RecursiveParser(get_groupbyterms))

groupby = Optional(KeywordParser(GROUP_BY) & groupbyterms)

simpleterm = SymbolsParser(STRING) | SymbolsParser(INTEGER) | \
    SymbolsParser(REALNUMBER) | SymbolsParser(DATE) | SymbolsParser(TIME) | \
    SymbolsParser(TIMESTAMP)

def get_expression():
    return expression

aggterm = \
    (KeywordParser(COUNT) & OperatorParser(L_PAR) & OperatorParser(STAR) & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(AVG) & OperatorParser(L_PAR) & RecursiveParser(get_expression) & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(MAX) & OperatorParser(L_PAR) & RecursiveParser(get_expression) & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(MIN) & OperatorParser(L_PAR) & RecursiveParser(get_expression) & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(SUM) & OperatorParser(L_PAR) & RecursiveParser(get_expression) & \
        OperatorParser(R_PAR))

term = \
    (OperatorParser(L_PAR) & RecursiveParser(get_expression) & \
        OperatorParser(R_PAR)) | \
    colref | \
    simpleterm | \
    aggterm

neg = \
    term | \
    (OperatorParser(PLUS) & term) | \
    (OperatorParser(MINUS) & term)

times = \
    neg & ZeroOrMore((OperatorParser(STAR) | OperatorParser(DIV)) & neg)

expression = \
    times & ZeroOrMore((OperatorParser(PLUS) | OperatorParser(MINUS)) & times)

def get_selectlist():
    return selectlist
selectlist = \
    (expression & OperatorParser(COMMA) & RecursiveParser(get_selectlist)) | \
    expression

selectallcols = Optional(KeywordParser(ALL) | KeywordParser(DISTINCT))
selectcols = (selectallcols & OperatorParser(STAR)) | \
    (selectallcols & selectlist)

column = columnname
def get_columnlist():
    return columnlist
columnlist = column & RecursiveParser(get_columnlist) | column

def get_valuelist():
    return valuelist
valuelist = \
    (KeywordParser(NULL) & OperatorParser(COMMA) & \
        RecursiveParser(get_valuelist)) | \
    (expression & OperatorParser(COMMA) & RecursiveParser(get_valuelist)) | \
    expression | \
    KeywordParser(NULL)

insertvals = \
    (OperatorParser(L_PAR) & columnlist & OperatorParser(R_PAR) & \
        KeywordParser(VALUES) & OperatorParser(L_PAR) & valuelist & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(VALUES) & OperatorParser(L_PAR) & valuelist & \
        OperatorParser(R_PAR))

set_statement = \
    (column & OperatorParser(ASSIGNMENT) & KeywordParser(NULL)) | \
    (column & OperatorParser(ASSIGNMENT) & expression)
setlist = \
    set_statement & ZeroOrMore(OperatorParser(COMMA) & set_statement)

pattern = SymbolsParser(STRING)

op = OperatorParser(GT) | OperatorParser(GE) | OperatorParser(LE) | \
    OperatorParser(LQ) | OperatorParser(ASSIGNMENT) | OperatorParser(DIFFERENT)

def get_boolean():
    return boolean
comparison = \
    (OperatorParser(L_PAR) & RecursiveParser(get_boolean) & OperatorParser(R_PAR) & colref & \
        KeywordParser(IS) & KeywordParser(NULL)) | \
    (colref & KeywordParser(IS) & KeywordParser(NOT) & KeywordParser(NULL)) | \
    (expression & KeywordParser(LIKE) & pattern) | \
    (expression & KeywordParser(NOT) & KeywordParser(LIKE) & pattern) | \
    (expression & KeywordParser(IN) & OperatorParser(L_PAR) & valuelist & \
        OperatorParser(R_PAR)) | \
    (expression & KeywordParser(NOT) & KeywordParser(IN) & OperatorParser(L_PAR) & \
        valuelist & OperatorParser(R_PAR)) | \
    (expression & op & expression)

not_statement = comparison | (KeywordParser(NOT) & comparison)

def get_and_statement():
    return and_statement
and_statement = \
    not_statement | \
    (not_statement & KeywordParser(AND) & RecursiveParser(get_and_statement))

def get_boolean():
    return boolean
boolean = and_statement | (and_statement & KeywordParser(OR) & \
    RecursiveParser(get_boolean))

where = Optional(KeywordParser(WHERE) & boolean)
having = Optional(KeywordParser(HAVING) & boolean)

select_statement = \
    selectcols & \
    KeywordParser(FROM) & \
    tablelist & \
    where & \
    groupby & \
    having & \
    orderby
delete_statement = \
    KeywordParser(FROM) & \
    table & \
    where
insert_statement = \
    KeywordParser(INTO) & \
    table & \
    insertvals
update_statement = \
    table & \
    KeywordParser(SET) & \
    setlist & \
    where

def get_and_statement():
    return and_statement

statement = \
     (KeywordParser(SELECT) & select_statement & Optional(OperatorParser(SEMICOLON))) | \
     (KeywordParser(INSERT) & insert_statement & Optional(OperatorParser(SEMICOLON))) | \
     (KeywordParser(DELETE) & delete_statement & Optional(OperatorParser(SEMICOLON))) | \
     (KeywordParser(UPDATE) & update_statement & Optional(OperatorParser(SEMICOLON)))

sql_script = AllTokensConsumed(Repeat(statement))

# obtain list of tokens
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(sql)
print(tokens)

# build AST
result = sql_script(tokens, 0)
result.pretty_print()
