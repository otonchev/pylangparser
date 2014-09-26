# pylangparser.py
#
# Copyright (C) 2014 Ognyan Tonchev <otonchev at gmail.com>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
# Boston, MA 02110-1301, USA.
#

# pylangparser - provides classes for parsing formal languages
# in an easy way. There is a Lexer and a Parser. The lexer produces
# list of tokens that the Parser then uses to build the AST. The lexer
# can also be used as a stand alone component.
# The grammars are defined directly into the Python code. For details,
# check the unit tests and the examples.
#
# Note: the Parser is under development and is not fully committed
# yet. However, the Lexer is fully functional.
#

__version__ = "0.1"
__author__ = "Ognyan Tonchev"
__author_email__ = "otonchev@gmail.com"

import re
import unittest

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
        raise NotImplementedError("Method should be implemented")

    def matchToken(self, input_string):
        """ Checks whether input_string is a valid token """
        for token in self._get_tokens():
            if re.match('^' + token.get_pattern() + '$', input_string):
                if not input_string.endswith('\n') or \
                    token.get_pattern().endswith('\\n'):
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
        Returns a list of tokens present in the source. The list is in the
        format:
        [('token1', Token), ('token2', Token),...]
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

class ParserResult:
    """
    TokenParser's use this class to accumulate result from parsing the
    token list returned by the Lexer
    """
    def __init__(self, token, pos):
        self.__token = token
        self.__position = pos

    def __repr__(self):
        return '(%s, %d)' % (self.__token, self.__position)

    def get_position(self):
        """ get the position of the next token in the list """
        return self.__position

    def get_token(self):
        """ get the token of this ParserResult """
        return self.__token

class TokenParser:
    """
    Base class for all token parsers used to build the AST.
    The idea is that each parser consumes just one token from the
    token list. Parsers can be combined together in order to parse
    sequence of tokens.
    Then the group of parsers can be run onto the token sequence,
    note the __call__ method below.
    The result from the parsing is accumulated in a ParserResult.
    """

    def __call__(self, tokens, pos):
        raise NotImplementedError("Method should be implemented")

    def __and__(self, right):
        return CombineParsers(self, right)

    def __or__(self, right):
        return SelectParser(self, right)

class CombineParsers(TokenParser):
    """
    This class is used to combine two token parsers using the AND(&)
    operator. Both parsers should succeed consuming tokens from the
    token list in order for the whole combination to succeed.
    """

    def __init__(self, first, second):
        self.first = first
        self.second = second

    def __call__(self, tokens, pos):
        first_res = self.first(tokens, pos)
        if first_res:
            second_res = self.second(tokens, first_res.get_position())
            if second_res:
                return ParserResult((first_res, second_res), \
                    second_res.get_position())
        return None

class SelectParser(TokenParser):
    """
    This class is used to combine two token parsers using the OR(|)
    bitwise operator. One of the parsers should succeed consuming a token
    from the token list in order for the whole combination to succeed.
    """

    def __init__(self, first, second):
        self.first = first
        self.second = second

    def __call__(self, tokens, pos):
        res = self.first(tokens, pos)
        if res:
            return ParserResult(res, res.get_position())
        res = self.second(tokens, pos)
        if res:
            return ParserResult(res, res.get_position())
        return None

class KeywordParser(TokenParser):
    """
    This parser consumes a Keyword from the grammar.
    """

    def __init__(self, token):
        self.__token = token

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        (token, instance) = tokens[pos]
        if instance == self.__token:
            return ParserResult(token, pos + 1)
        return None

class OperatorParser(TokenParser):
    """
    This parser consumes an Operator from the grammar.
    """

    def __init__(self, token):
        self.__token = token

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        (token, instance) = tokens[pos]
        if instance == self.__token:
            return ParserResult(token, pos + 1)
        return None

class SymbolsParser(TokenParser):
    """
    This parser consumes Symbols from the grammar.
    """

    def __init__(self, instance):
        self.__instance=instance

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        (token, instance) = tokens[pos]
        if instance == self.__instance:
            return ParserResult(token, pos + 1)
        return None

class Optional(TokenParser):
    """
    This class marks a TokenParser to be optional. This means
    that the parser will not fail even if it does not find a token
    to consume.
    """

    def __init__(self, parser):
        self.__parser = parser

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        result = self.__parser(tokens, pos)
        if result:
            return result
        return ParserResult(None, pos)

class ZeroOrMore(TokenParser):
    """
    This class marks a TokenParser to be optional and appear multiple
    times. The parser will be applied multiple times until it fails to
    consume token.
    """

    def __init__(self, parser):
        self.__parser = parser

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        results = None
        while pos < len(tokens):
            result = self.__parser(tokens, pos)
            if not result:
                break
            pos = result.get_position()
            if not results:
                results = (result,)
            else:
                results = results + (result,)
        return ParserResult(results, pos)

class Repeat(TokenParser):
    """
    This class allows a TokenParser to appear multiple times. The parser
    will be applied multiple times until it fails to consume token.
    """

    def __init__(self, parser):
        self.__parser = parser

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        results = None
        while pos < len(tokens):
            result = self.__parser(tokens, pos)
            if not result:
                break
            pos = result.get_position()
            if not results:
                results = (result,)
            else:
                results = results + (result,)
        if not results:
            return None
        return ParserResult(results, pos)

class AllTokensConsumed(TokenParser):
    """
    This class makes sure that all input tokens are consumed.
    """
    def __init__(self, parser):
        self.__parser = parser

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        result = self.__parser(tokens, pos)
        if not result:
            return None
        pos = result.get_position()
        if pos != len(tokens):
            return None
        return result

class RecursiveParser(TokenParser):
    """
    This class is used for parsing recursive grammars.
    Example:

    ifstmt = if + ( + cond + ) + { + ifstmt  + }
    """

    def __init__(self, get_parser_func):
        self.__get_parser = get_parser_func

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        parser = self.__get_parser()
        return parser(tokens, pos)


class ParseTests(unittest.TestCase):
    """ Follow the unit tests for the Lexer and the Parser """

    def __init__(self, *args, **kwargs):

        # define all tokens in the language
        self.FOR = Keyword(r'for')
        self.RETURN = Keyword(r'return')
        self.IF = Keyword(r'if')

	self.COMMA = Operator(r',')
	self.COLON = Operator(r';')
	self.ASSIGNMENT = Operator(r'=')
	self.LBRACKET = Operator(r'\(')
	self.RBRACKET = Operator(r'\)')
	self.LBRACE = Operator(r'{')
	self.RBRACE = Operator(r'}')
	self.AND = Operator(r'&')
	self.POINTER = Operator('\*')
	self.PP = Operator(r'\+\+')
	self.LE = Operator(r'<=')

	self.IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
	self.INT_IDENTIFIER = Symbols(r'[0-9]*')
	self.STRING_IDENTIFIER = Symbols(r'\".*\"')

	self.C_STYLE_COMMENT = Ignore(r'/\*.*\*/')
	self.CPP_STYLE_COMMENT = Ignore(r'//.*\n')
	self.MACROS = Ignore(r'\#.*\n')
	self.IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

        # group tokens into sub-groups
        self.IGNORES = self.C_STYLE_COMMENT & self.CPP_STYLE_COMMENT & \
            self.MACROS & self.IGNORE_CHARS

        self.KEYWORDS = self.FOR & self.RETURN & self.IF

        self.OPERATORS = self.COMMA & self.COLON & self.ASSIGNMENT & \
            self.LBRACKET & self.RBRACKET & self.LBRACE & self.RBRACE & \
            self.AND & self.POINTER & self.PP & self.LE

        self.IDENTIFIERS = self.IDENTIFIER & self.INT_IDENTIFIER & \
            self.STRING_IDENTIFIER

        # join all token sub-groups
        self.TOKENS = self.KEYWORDS & self.OPERATORS & self.IDENTIFIERS & \
            self.IGNORES

        super(ParseTests, self).__init__(*args, **kwargs)

    def __checkEntry(self, expected_token, expected_instance, touple):
        (token, instance) = touple
        self.assertEqual(expected_token, token)
        self.assertEqual(expected_instance, instance)

    def testLexer(self):

        # our source code
        source = r"""

        #include <stdio.h>

        // alabala

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
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(source)

        self.__checkEntry("long", self.IDENTIFIER, tokens[0])
        self.__checkEntry("factorial", self.IDENTIFIER, tokens[1])
        self.__checkEntry("(", self.LBRACKET, tokens[2])
        self.__checkEntry("int", self.IDENTIFIER, tokens[3])
        self.__checkEntry("n", self.IDENTIFIER, tokens[4])
        self.__checkEntry(")", self.RBRACKET, tokens[5])
        self.__checkEntry("{", self.LBRACE, tokens[6])
        self.__checkEntry("int", self.IDENTIFIER, tokens[7])
        self.__checkEntry("c", self.IDENTIFIER, tokens[8])
        self.__checkEntry(";", self.COLON, tokens[9])
        self.__checkEntry("long", self.IDENTIFIER, tokens[10])
        self.__checkEntry("result", self.IDENTIFIER, tokens[11])
        self.__checkEntry("=", self.ASSIGNMENT, tokens[12])
        self.__checkEntry("1", self.INT_IDENTIFIER, tokens[13])
        self.__checkEntry(";", self.COLON, tokens[14])
        self.__checkEntry(";", self.COLON, tokens[33])
        self.__checkEntry("return", self.RETURN, tokens[34])
        self.__checkEntry("result", self.IDENTIFIER, tokens[35])
        self.__checkEntry(";", self.COLON, tokens[36])
        self.__checkEntry("}", self.RBRACE, tokens[37])

    def testParser(self):

        # our source code
        source = r"""

        return 5;

        """

        # obtain a list of all tokens present in the source
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(source)

        parser = AllTokensConsumed(KeywordParser(self.RETURN) & \
            SymbolsParser(self.INT_IDENTIFIER) & OperatorParser(self.COLON))
        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)

        parser = (KeywordParser(self.RETURN) | OperatorParser(self.FOR)) & \
            SymbolsParser(self.INT_IDENTIFIER)
        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)

        parser = Optional(KeywordParser(self.RETURN))
        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)

        parser = ZeroOrMore(KeywordParser(self.RETURN))
        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)

    def testRecursiveParser(self):

        # our source code
        source = r"""

        if (5) {
          if (5) {
            if (5) {
            }
          }
        }

        """

        # obtain a list of all tokens present in the source
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(source)

        def return_parser():
            return parser

        parser = KeywordParser(self.IF) & OperatorParser(self.LBRACKET) & \
            SymbolsParser(self.INT_IDENTIFIER) & OperatorParser(self.RBRACKET) & \
            OperatorParser(self.LBRACE) & Optional(RecursiveParser(return_parser)) & \
            OperatorParser(self.RBRACE)
        
        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)

    def testRecursiveParser2(self):

        # our source code
        source = r"""

        a, a, a, a

        """

        # obtain a list of all tokens present in the source
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(source)

        def return_parser():
            return parser

        parser = AllTokensConsumed(Repeat((KeywordParser(self.COMMA) & \
            RecursiveParser(return_parser)) | SymbolsParser(self.IDENTIFIER)))

        result = parser(tokens, 0)
        self.assertTrue(result)
        print(result)

def main():
    unittest.main()

if __name__ == '__main__':
    main()
