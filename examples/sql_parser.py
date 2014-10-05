# Example usage of the Parser. The test parses SQL scrips
# and validates the grammar.
#
# Copyright, 2014, Ognyan Tonchev (otonchev at gmail com)
#

# this is a working incomplete version

statement = \
     (KeywordParser(SELECT) & select_statement) | \
     (KeywordParser(INSERT) & insert_statement) | \
     (KeywordParser(DELETE) & delete_statement) | \
     (KeywordParser(UPDATE) & update_statement)

tablename = SymbolsParser(IDENTIFIER)
columnname = SymbolsParser(IDENTIFIER)

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
insert_stamenet = \
    KeywordParser(INTO) & \
    table & \
    insertvals
update__statement = \
    table & \
    KeywordParser(SET) & \
    setlist & \
    where

setlist = \
    set_statement & ZeroOrMore(OperatorParser(COMMA) & set_statement)
set_statement = \
    (column & OpretaorParser(ASSIGNMENT) & SymbolsParser(NULL)) | \
    (column & OpretaorParser(ASSIGNMENT) & expression)

insertvals = \
    (OperatorParser(L_PAR) & columnlist & OperatorParser(R_PAR) & \
        KeywordParser(VALUES) & OperatorParser(L_PAR) & valuelist & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(VALUES) & OperatorParser(L_PAR) & valuelist & \
        OperatorParser(R_PAR))

def get_columnlist():
    return columnlist
columnlist = column & RecursiveParser(get_columnlist) | column
column = columnname

def get_valuelist():
    return valuelist
valuelist = \
    (SymbolsParser(NULL) & OperatorParser(COMMA) & \
        RecursiveParser(get_valuelist)) | \
    (expression & OperatorParser(COMMA) & RecusrsiveParser(get_valuelist)) | \
    expression | \
    SymbolsParser(NULL)

selectcols = (selectallcols & OperatorParser(STAR)) | \
    (selectallcols & selectlist)
selectallcols = KeywordParser(ALL) | KeywordParser(DISTINCT)
selectlist = (expression & OperatorParser(COMMA) & selectlist) | expression

where = KeywordParser(WHERE) & boolean
having = KeywordParser(HAVING) & boolean

def get_boolean():
    return boolean
boolean = and_statement | (and_statement & KeywordParser(OR) & \
    RecursiveParser(get_boolean))

def get_and_statement():
    return and_statement

and_statement = \
    not_statement | \
    (not_statement & KeywordParser(AND) & RecursiveParser(get_and_statement))

not_statement = comparison | (KeywordParser(NOT) & comparison)

comparison = \
    (OperatorParser(L_PAR) & boolean & OperatorParser(R_PAR) & colref & \
        KeywordParser(IS) & SymbolsParser(NULL)) | \
    (colref & KeywordParser(IS) & KeywordParser(NOT) & SymbolsParser(NULL)) | \
    (expression & KeywordParser(LIKE) & pattern) | \
    (expression & KeywordParser(NOT) & KeywordParser(LIKE) & pattern) | \
    (expression & KeywordParser(IN) & OperatorParser(L_PAR) & valuelist & \
        OperatorParser(R_PAR)) | \
    (expression & KeywordParser(NOT) & KeywordParser(IN) & OperatorParser(L_PAR) & \
        valuelist & OperatorParser(R_PAR)) | \
    (expression & op & expression)

op = GT | GE | LE | LE | ASSIGNMENT | DIFFERENT

pattern = SymbolsPaarser(STRING)
expression = \
    (expression & OperatorParser(PLUS) & times) | \
    (expression & OperatorParser(MINUS) & times) | \
    times

times = \
    (times & OperatorParser(STAR) & neg) | \
    (times & OperatorParser(DIV) & neg) | \
    neg

neg = \
    term | \
    (OperatorParser(PLUS) & term) | \
    (OperatorParser(MINUS) & term)

term = \
    (OperatorParser(L_PAR) & expression & OperatorParser(R_PAR)) | \
    colref | \
    simpleterm | \
    aggterm

aggterm = \
    (KeywordParser(COUNT) & OperatorParser(L_PAR) & OperatorParser(STAR) & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(AVG) & OperatorParser(L_PAR) & expression & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(MAX) & OperatorParser(L_PAR) & expression & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(MIN) & OperatorParser(L_PAR) & expression & \
        OperatorParser(R_PAR)) | \
    (KeywordParser(SUM) & OperatorParser(L_PAR) & expression & \
        OperatorParser(R_PAR))

simpleterm = string | realnumber | date | time | timestamp

groupby = KeywordParser(GROUP_BY) & groupbyterms

def get_groupbyterms():
    return groupbyterms
groupbyterms = \
    colref | \
    (colref & OperatorParser(COMMA) & RecursiveParser(get_groupbyterms))

orderby = KeywordParser(ORDER_BY) & orderbyterms

def get_orderbyterms():
    return orderbyterms
orderbyterms = orderbyterm | \
    (orderbyterm & OperatorParser(COMMA) & RecursiveParser(get_orderbyterms))

orderbyterm = (colref & asc) | (integer & asc)

asc = KeywordParser(ASC) | KeywordParser(DESC)

colref = (aliasname & OperatorParser(COMMA) & columnname) | columnname

aliasname = SymbolsParser(IDENTIFIER)

tablelist = (tableref & OperatorParser(COMMA) & tablelist) | tableref

tableref = table | (table & aliasname)

table = tablename


identifier ::= an identifier (identifiers containing spaces must be enclosed in double quotation marks)
string ::= a string (enclosed in single quotation marks)
realnumber ::= a non-negative real number
integer ::= a non-negative integer
date ::= a date in ODBC escape clause format (for example, {d'1996-02-05'} or
   --(*vendor(Microsoft),product(ODBC) d'1996-02-05'*)--
time ::= a time in ODBC escape clause format (for example, {t'10:19:48'} or
   --(*vendor(Microsoft),product(ODBC) t '10:19:48'*)--
timestamp ::= a timestamp in ODBC escape clause format (for example,
  {ts'1996-02-05 10:19:48.529'} or
  --(*vendor(Microsoft),product(ODBC) ts '1996-02-05 10:19:48.529"*)--
