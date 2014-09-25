import re

class ParseException(Exception):
    """
    Exception class for parsing exceptions. Holds line number
    and message for the source of error
    """

    def __init__(self, line=0, message=None):
        self.__line = line
        self.__message = message

    def __str__( self ):
        return "line: %d, message: %s" % (self.__line, self.__message)

class TokenMatcher():
    """ Interface for extracting tokens from a text """

    def _get_tokens(self):
        raise NotImplementedError("Should have implemented")

    def matchToken(self, input_string):
        """ Checks whether input_string is a valid token """
        if input_string.endswith('\n'):
            return None
        for token in self._get_tokens():
            if re.match('^' + token.get_pattern() + '$', input_string):
                return token
        return None

class TokenContainer(TokenMatcher):
    """ Interface for grouping tokens. Contains a list of grouped tokens """

    def __init__(self, obj):
        if not isinstance(obj, Token):
            raise TypeError("right argument must be Token")
        self.__tokens = [obj]

    def __and__(self, right):
        """
        Overrides the bitwise AND operator and merges two TokenContainer
        objects or a TokenContainer object and a token into the first one.
        """
        if isinstance(right, TokenContainer):
            self.__tokens = self.__tokens + right.__tokens
        elif isinstance(right, Token):
            self.__tokens = self.__tokens + [right]
        else:
            raise TypeError("right argument must be Token or TokenContainer")
        return self

    def _get_tokens(self):
        return self.__tokens

class Token():
    """ Base class for all tokens in the grammar """

    def __init__(self, pattern):
        self.__pattern = pattern
        self._ignore = False

    def get_pattern(self):
        return self.__pattern

    def isIgnore(self):
        return self._ignore

    def __and__(self, right):
        return TokenContainer(self) & right

class Keyword(Token):
    """ Keyword token """
    pass

class Symbols(Token):
    """ Symbols token"""
    pass

class Operator(Token):
    """ Operator token """
    pass

class Ignore(Token):
    """ Valid token of no interest, should be ignored in parsing """
    def __init__(self, pattern):
        Token.__init__(self, pattern)
        self._ignore = True

class Lexer:
    """
    A lexer class. Extracts tokens from a file according to a pre-defined
    grammar
    """
    def __init__(self, token_matcher, ignore_chars=None):
        self.__token_matcher = token_matcher
        self.__ignore_chars = None

    def __findLineNumber(self, input_text, pos):
        return input_text.count("\n", 0, pos) + 1

    def parseTokens(self, input_text):
        """
        Returns a list of tokens present in the source.
        Raises ParseException
        """
        start = 0
        index = 1
        match = None
        tokens = []

        while start + index <= len(input_text):

            # check for tokens
            result = self.__token_matcher.matchToken(input_text[start:start + index])
            if result:
                # remember that we got a match
                match = result
            elif match:
                # previous token matched, current didn't, take previous
                if not match.isIgnore():
                    tokens.append((input_text[start:start + index - 1], match))
                start = start + index - 1
                index = 0
                match = None
            else:
                # no match at all
                match = None

            index = index + 1

            # ignore trailing newline
            while input_text[start:start + index].startswith('\n'):
                start = start + 1
                index = 1
                match = Ignore(r'\n')

        if match:
            if not match.isIgnore():
                tokens.append((input_text[start:start + index - 1], match))
        else:
            raise ParseException(self.__findLineNumber(input_text, start))

        return tokens

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
LBRACKET = Operator(r'\(')
RBRACKET = Operator(r'\)')
LBRACE = Operator(r'{')
RBRACE = Operator(r'}')
AND = Operator(r'&')
POINTER = Operator('\*')
PP = Operator(r'\+\+')
LE = Operator(r'<=')

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
INT_IDENTIFIER = Symbols(r'[0-9]*')
STRING_IDENTIFIER = Symbols(r'\".*\"')

C_STYLE_COMMENT = Ignore(r'/\*.*\*/')
CPP_STYLE_COMMENT = Ignore(r'//.*\n')

IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

# group tokens into sub-groups
KEYWORDS = AUTO & BREAK & CASE & ENUM & CONST & CONTINUE & DEFAULT & DO & ELSE & \
    EXTERN & FOR & GOTO & IF & REGISTER & RETURN & SIZEOF & STATIC & STRUCT & \
    SWITCH & UNION & VOLATILE & WHILE & ENUM & TYPEDEF & VOID & CHAR & SHORT & \
    INT & LONG & FLOAT & DOUBLE & SIGNED & UNSIGNED

OPERATORS = COMMA & COLON & ASSIGNMENT & LBRACKET & RBRACKET & LBRACE & RBRACE & \
    AND & POINTER & PP & LE

IDENTIFIERS = IDENTIFIER & INT_IDENTIFIER & STRING_IDENTIFIER

COMMENTS = C_STYLE_COMMENT & CPP_STYLE_COMMENT

# join all token sub-groups
TOKENS = KEYWORDS & OPERATORS & IDENTIFIERS & COMMENTS & IGNORE_CHARS







#enumerator_list = IDENTIFIER & (COMMA & IDENTIFIER).setZeroOrMore(True)

#enum_specifier = ENUM & LBRACE & enumerator_list & RBRACE | ENUM & IDENTIFIER & LBRACE & enumerator_list & RBRACE | ENUM & IDENTIFIER






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

"""

# obtain a list of all tokens present in the source
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(source)
print(tokens)
