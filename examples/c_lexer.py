# Example usage of the Lexer class. The test parses a C program
# and builds a list of tokens.
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

COMMA = Operator(r',')
COLON = Operator(r';')
ASSIGNMENT = Operator(r'=')
LBRACKET = Operator(r'(')
RBRACKET = Operator(r')')
LBRACE = Operator(r'{')
RBRACE = Operator(r'}')
AND = Operator(r'&')
POINTER = Operator('*')
PP = Operator(r'++')
LE = Operator(r'<=')
DIV = Operator('/')

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
INT_IDENTIFIER = Symbols(r'[0-9]*')
STRING_IDENTIFIER = Symbols(r'\".*\"')

CPP_STYLE_COMMENT = Ignore(r'//.*\n')
MACROS = Ignore(r'#.*\n')
IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

# group tokens into sub-groups
IGNORES = CPP_STYLE_COMMENT & MACROS & IGNORE_CHARS

KEYWORDS = AUTO & BREAK & CASE & ENUM & CONST & CONTINUE & DEFAULT & DO & ELSE & \
    EXTERN & FOR & GOTO & IF & REGISTER & RETURN & SIZEOF & STATIC & STRUCT & \
    SWITCH & UNION & VOLATILE & WHILE & ENUM & TYPEDEF & VOID & CHAR & SHORT & \
    INT & LONG & FLOAT & DOUBLE & SIGNED & UNSIGNED

OPERATORS = COMMA & COLON & ASSIGNMENT & LBRACKET & RBRACKET & LBRACE & RBRACE & \
    AND & POINTER & PP & LE & DIV

IDENTIFIERS = IDENTIFIER & INT_IDENTIFIER & STRING_IDENTIFIER

# join all token sub-groups
TOKENS = KEYWORDS & OPERATORS & IDENTIFIERS & IGNORES


# our source code
source = r"""

long factorial(int);
 
int main()
{
  int number;
  long fact = 1;
 
  printf("Enter a number to calculate it's factorial\n");
  /* read number from stdin */
  scanf("%d", &number);
 
  printf("%d! = %ld\n", number, factorial(number));
 
  return 0;
}

/* factorial */
long factorial(int n)
{
  int c;
  long result = 1;
 
  for (c = 1; c <= n; c++)
    result = result * c;
 
  return result;
}
/* comment */

"""

# obtain a list of all tokens present in the source
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(source)
print(tokens)
