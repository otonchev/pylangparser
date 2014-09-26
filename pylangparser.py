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
# Note: the parser is under development and is not included in the
# source yet.
#

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


class ParseTests(unittest.TestCase):

    def __init__(self, *args, **kwargs):

        # define all tokens in the language
        self.FOR = Keyword(r'for')
        self.RETURN = Keyword(r'return')

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

        self.KEYWORDS = self.FOR & self.RETURN

        self.OPERATORS = self.COMMA & self.COLON & self.ASSIGNMENT & \
            self.LBRACKET & self.RBRACKET & self.LBRACE & self.RBRACE & \
            self.AND & self.POINTER & self.PP & self.LE

        self.IDENTIFIERS = self.IDENTIFIER & self.INT_IDENTIFIER & \
            self.STRING_IDENTIFIER

        # join all token sub-groups
        self.TOKENS = self.KEYWORDS & self.OPERATORS & self.IDENTIFIERS & \
            self.IGNORES

	# our source code
	self.source = r"""

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

        super(ParseTests, self).__init__(*args, **kwargs)

    def __checkEntry(self, expected_token, expected_instance, touple):
        (token, instance) = touple
        self.assertEqual(expected_token, token)
        self.assertEqual(expected_instance, instance)

    def testLexer(self):
        # obtain a list of all tokens present in the source
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(self.source)

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

def main():
    unittest.main()

if __name__ == '__main__':
    main()
