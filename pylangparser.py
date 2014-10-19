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
# Note: the Parser is functional but still under development.
# Documentation is not complete yet. New APIs for building custom ASTs
# will be added soon.
#

__version__ = "0.1"
__author__ = "Ognyan Tonchev"
__author_email__ = "otonchev@gmail.com"

import re
import unittest
import pprint

class ParseException(Exception):
    """
    Exception class for parsing exceptions. Holds row number, column
    and message for the source of error
    """

    def __init__(self, row=0, column=0, message=None):
        self.__row = row
        self.__column =  column
        self.__message = message

    def get_row(self):
        """ get row of the data that failed to parse """
        return self.__row

    def get_column(self):
        """ get column of the data that failed to parse """
        return self.__column

    def get_message(self):
        """ get message of the failure """
        return self.__message

    def __str__( self ):
        return "row: %d, column: %d, message: %s" % (self.__row, \
            self.__column, self.__message)

class TokenIterator():
    """ Common interface for classes that want to iterate tokens """

    def _get_tokens(self):
        raise NotImplementedError("Method should be implemented")

class TokenMatcher(TokenIterator):
    """ Interface for extracting tokens from a text """

    def match_token(self, input_string):
        """ Checks whether input_string is a valid token """
        for token in self._get_tokens():

            flags = 0

            pattern = token.get_pattern()
            if token.is_autoescape():
                pattern = re.escape(pattern)
            if token.is_ignorecase():
                flags = re.IGNORECASE

            if re.match('^' + pattern + '$', input_string, flags):
                if not input_string.endswith('\n') or \
                    token.get_pattern().endswith('\\n'):
                    return token
        return None

class TokenSetter(TokenIterator):
    """ Interface for setting a property on a group of tokens """

    def set_token(self, tokens, set_func, set_func_data):
        """
        Call token.set_func(set_func_data).
        If argument is a list of tokens (TokenContainer) this function
        will call set_func(set_func_data) for each token in the group
        """
        if isinstance(tokens, TokenContainer):
            for token in self._get_tokens():
                set_func(token, set_func_data)
        else:
            raise TypeError("argument must be TokenContainer")

class TokenContainer(TokenMatcher, TokenSetter):
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

    def __init__(self, pattern, ignorecase=False):
        """
        pattern - pattern for the token, it can be a string or a regular
            expression(r'for', r'>=' or r'[a-z]+')
        ignorecase - whether case-insensitive matching should be performed
            when parsing tokens
        """
        self.__row = 0
        self.__column = 0
        self.__pattern = pattern
        self.__ignore_ast = False
        self.__ignore_case = ignorecase
        self._ignore = False
        self._autoescape = False

    def get_pattern(self):
        """ Get the pattern for this token """
        return self.__pattern

    def is_ignorecase(self):
        """
        Check whether case-insensitive matching should be performed when
        parsing tokens
        """
        return self.__ignore_case

    def is_ignore(self):
        """
        Check if token should be ignored when building the list of tokens
        which a Lexer produces
        """
        return self._ignore

    def is_autoescape(self):
        """
        Check if pattern should be autoescaped before matching it with a
        string
        """
        return self._autoescape

    def set_ignore_ast(self, value):
        """ Set whether the Token should be ignored when building the AST """
        self.__ignore_ast = value

    def get_ignore_ast(self):
        """ Check whether the Token will be ignored when building the AST """
        return self.__ignore_ast

    def __and__(self, right):
        """ Overriding operator & allows for grouping tokens together """
        return TokenContainer(self) & right

    def __repr__(self):
        return self.__pattern

class Keyword(Token):
    """ Keyword token """
    pass

class Symbols(Token):
    """ Symbols token"""
    pass

class Operator(Token):
    """ Operator token """
    def __init__(self, pattern):
        Token.__init__(self, pattern)
        self._autoescape = True

class Ignore(Token):
    """ Valid token of no interest, should be ignored in parsing """
    def __init__(self, pattern, ignorecase=False):
        Token.__init__(self, pattern, ignorecase)
        self._ignore = True

class Lexer:
    """
    A lexer class. Extracts tokens from a file according to a pre-defined
    grammar
    """
    C_STYLE_COMMENT = r'/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/'

    class TokenData:
        """
        represents one token in the list of tokens returned by the Lexer
        """
        def __init__(self, token_str, token_instance, row, column):
            self.__token_str = token_str
            self.__token_instance = token_instance
            self.__touched = False
            self.__row = row
            self.__column = column

        def __repr__(self):
            return '(' + self.__token_str + ', ' + str(self.__token_instance) + ')'

        def touch(self):
            """ mark token as successfully parsed when building AST """
            self.__touched = True

        def is_touched(self):
            """ check if token was successfully parsed when building AST """
            return self.__touched

        def get_token(self):
            """ token string """
            return self.__token_str

        def get_instance(self):
            """ token instance: Keyword, Operator, Symbol """
            return self.__token_instance

        def get_data(self):
            """ token string and instance as a touple """
            return (self.__token_str, self.__token_instance)

        def get_row(self):
            """ row in source where token is found """
            return self.__row

        def get_column(self):
            """ column in source where token is found """
            return self.__column

    def __init__(self, token_matcher):
        self.__token_matcher = token_matcher

    def __calculate_new_pos(self, input_text, start, prev_start, row, column):
        """ calculate (row, pos) for current token """
        row = row + input_text[prev_start:start].count('\n')
        last_nl_index = input_text[prev_start:start].rfind('\n')
        if last_nl_index > -1:
            column = start - (prev_start + last_nl_index)
        else:
            column = column + (start - prev_start)
        prev_start = start
        return (row, column)

    def parseTokens(self, input_text):
        """
        Returns a list of tokens present in the source. The list is in the
        format:
        [TokenData, TokenData,...]
        Raises ParseException
        """
        start = 0
        token_len = 1
        match = None
        tokens = []

        row = 0
        column = 0
        prev_start = 0

        while start + token_len <= len(input_text):

            # ignore C style comments
            m = re.match('^' + self.C_STYLE_COMMENT, input_text[start:], \
                re.DOTALL)
            if m:
                start = start + m.end(0)
                token_len = 1

            # check for tokens
            result = \
                self.__token_matcher.match_token(input_text[start:start + token_len])
            if result:
                # remember that we got a match
                match = result
            elif match:
                # previous token matched, current didn't, take previous
                if not match.is_ignore():
                    # calculate row and column for the new token
                    (row, column) = self.__calculate_new_pos(input_text, start, \
                        prev_start, row, column)
                    prev_start = start
                    data = self.TokenData(input_text[start:start + token_len - 1], match, \
                        row, column)
                    # and append it to the result
                    tokens.append(data)
                start = start + token_len - 1
                token_len = 0
                match = None
            else:
                # no match at all
                match = None

            token_len = token_len + 1

            # ignore trailing newline
            while input_text[start:start + token_len].startswith('\n'):
                start = start + 1
                token_len = 1
                match = Ignore(r'\n')

        if match:
            if not match.is_ignore():
                tokens.append((input_text[start:start + token_len - 1], match))
        else:
            (row, column) = self.__calculate_new_pos(input_text, start, \
                        prev_start, row, column)
            raise ParseException(row, column, "Error parsing token")

        return tokens

def IgnoreTokensInAST(tokens):
    """
    specify which tokens should be ignored in the AST. Argument can be
    a simple token or a group of tokens.

    Example:

    IgnoreTokensInAST(SEMICOLON & LBRACE & RBRACE)

    In this case ';', '{' and '}' will not be present in the final AST

    Or:

    IgnoreTokensInAST(SEMICOLON)

    In this case ';' will not be present in the final AST

    """
    if isinstance(tokens, TokenContainer):
        tokens.set_token(tokens, Token.set_ignore_ast, True)
    else:
        tokens.set_ignore_ast(True)

class ParserResult:
    """
    TokenParser's use this class to accumulate result from parsing the
    token list returned by the Lexer.
    """
    def __init__(self, token, pos, instance, unpack=False):
        # __token:          is either a string, a ParserResult, or a tuple of
        #     ParserResult's
        # __position:       is the position of the next token in the lex list
        # __token_instances: is a list of Parser instances that generated the
        #     ParserResult. For basic tokens where __token is a string the list
        #     will contain just one entry (Keyword, Symbols, Operator, etc).
        #     For complex tokens such as ParserResult or touple of
        #     ParserResult'a it is a list of all Parser's which were
        #     successfully applied.
        # __unpack:         whether to unpack this result before grouping it
        #     with other ParserResult's or before analysing it
        self.__token = token
        self.__position = pos
        self.__token_instances = [instance]
        self.__unpack = unpack

    def __repr__(self):
        if not self.is_basic_token():
            #
            # this is a ParserResult or a tuple containing group of
            # ParserResult's
            #
            if isinstance(self.__token, tuple):
               string = None
               for value in self.__token:
                   if not string:
                       string = str(value)
                   else:
                       string = string + ', ' + str(value)
               return '(%s)' % string
            else:
               return '(%s)' % self.__token
        else:
            #
            # only primitive ParserResults have instance. They are in the
            # format: (token, pos, token_instance)
            # these are Reserved words, Symbols, etc
            #
            if isinstance(self.__token, tuple):
                raise TypeError("value must be str")
            return '(%s, instance: %s)' % (self.__token, \
                self.__token_instances[0])

    def unpack(self):
        """
        whether to unpack this result before grouping it with other
        ParserResult's or before abalysing it
        """
        return self.__unpack

    def is_basic_token(self):
        """
        check if token in ParserResult is a basic token, this are all
        tokens which are result of applying just one basic parser
        (Keyword, Symbols, Operator, etc).
        In this case self.__token is a string
        """
        return (isinstance(self.__token, str))

    def to_list(self):
        """ convert AST to list of strings for debug purposes """
        if isinstance(self.__token, str):
            return [self.__token]
        results = []
        for result in self.__token:
            if isinstance(result, ParserResult):
                results.append(result.to_list())
            else:
                results.append(result)
        return results

    def pretty_print(self, *args, **kwargs):
        """
        use Data pretty printer to print AST in human readable format

        Example output:

        [['for'],
         [['i'], ['='], ['5']],
         [['i'], ['<'], ['5']],
         [['i'], ['++']],
          [[[['p'], ['='], ['1']]],
          [['if'], [['i'], ['=='], ['4']], [['break']]]
        ]]
        """
        pprint.pprint(self.to_list(), *args, **kwargs)

    def get_position(self):
        """ get the position of the next token in the list """
        return self.__position

    def set_position(self, pos):
        """
        set the position of the next token in the list. This function
        is normally used by parser when building AST and ignoring tokens.
        """
        self.__position = pos

    def get_token(self):
        """
        get the token of this ParserResult. It is either a string, a single
        ParserResult or a tuple of ParserResult's
        """
        return self.__token

    def set_token(self, token):
        """
        set the token of this ParserResult. This function is normally used
        internally when building custom AST's
        """
        self.__token = token

    def add_instance(self, instance):
        """
        adds a new TokenParser instance to the ParserResult. This method is
        used when building AST's and is called for each and every TokenParser
        that was applied when the ParserResult was generated.

        Example:
            parser = KeywordParser(FOR) | KeywordParser(WHILE)
        after applying this parser the resulting ParserResult will have just
        one insance, it will be one of 'FOR' and 'WHILE'

            parser = KeywordParser(FOR) | KeywordParser(WHILE)
            parser1 = Repeat(parser)
        after applying this parser the resulting ParserResult will contain two
        instances, the first one will be one of 'FOR' and 'WHILE' and the second one
        will be 'Repeat'

        Instances are useful when iterating the AST to quickly check the type
        of a sub-tree.

        """
        if not isinstance(instance, TokenParser):
            raise TypeError("argument must be a TokenParser")
        self.__token_instances.append(instance)

    def is_instance(self, aimed):
        """
        check if a TokenParser was successfully applied when generating the
        ParserResult. Check self.add_instance() for more details.
        This method is useful when iterating the AST to quickly check the type
        of a sub-tree.
        """
        for instance in self.__token_instances:
            if aimed == instance:
                return True
        return False

    def get_instances(self):
        """
        get a list of all TokenParser's which were applied when generating the
        ParseResult. This function is useful for debug purposes.
        List can be printed as string, set name for each parser in the grammar
        in order to get pretty info:

            parser = OperatorParser(IF).set_name("IF") | \
                OperatorParser(WHILE).set_name("WHILE")
            parser &= "parser_name" # here we set a name for the top level parser
            result = parser(tokens, 0)
            print("applied parsers: %s" % result.get_instances())
        """
        return self.__token_instances

    def is_empty(self):
        """ check if ParserResult contains empty token """
        return (self.__token == None)

    def is_ignore(self):
        """ whether ParserResult should be ignored when building AST """
        if self.__token_instances[0] and \
            isinstance(self.__token_instances[0], Token):
            return self.__token_instances[0].get_ignore_ast()
        return False

    def get_sub_group(self, index):
        """
        returns a sub-ParserResult of type ParserResult, if index is not found,
        None value is returned
        sub-groups are indexed from 1, sub-group 0 is the whole AST
        """
        if index == 0:
            return self
        if isinstance(self.__token, ParserResult):
            return self.__token.get_sub_group(index - 1)
        if isinstance(self.__token, tuple):
            for value in self.__token:
                index = index - 1
                if index == 0:
                    return value
        return None

class TokenParser:
    """
    Base class for all token parsers used to build the AST.
    The idea is that each parser consumes just one token from the
    token list. Parsers can be combined together in order to parse
    sequence of tokens.
    Then the group of parsers can be run onto the token sequence,
    note the __call__ method below.
    The result from the parsing algorithm is accumulated in a
    ParserResult.

        parser = parser1 & parser2 & (parser3 | parser4)
        parser_result = parser(tokens_list, 0)
        parser_result.pretty_print()
    """
    def __init__(self):
        self.__name = "<no_name_set>"

    def __call__(self, tokens, pos):
        raise NotImplementedError("Method should be implemented")

    def __and__(self, right):
        return CombineManyParsers(self, right)

    def __lshift__(self, right):
        return CombineTwoParsers(self, right)

    def __or__(self, right):
        return SelectParser(self, right)

    def __iand__(self, right):
        """
        set name for a given parser by using the '&=' operator. This is useful
        for debug purposes only.

            parser = OperatorParser(IF) | OperatorParser(WHILE)
            parser &= "parser_name" # here we set a name for the top level parser

        Each ParserResult contains a list of all parsers that were applied when it
        was generated. To print the list simply use:

            result = parser(tokens, 0)
            print("applied parsers: %s" % result.get_instances())
        """
        self.__name = right
        print "set: %s, %s" % (self.__name, self)
        return self

    def set_name(self, name):
        """
        set name for a given parser. This is useful for debug purposes only.

            parser = OperatorParser(IF).set_name("IF") | \
                OperatorParser(WHILE).set_name("WHILE")

        Each ParserResult contains a list of all parsers that were applied when it
        was generated. To print the list simply use:

            result = parser(tokens, 0)
            print("applied parsers: %s" % result.get_instances())
        """
        self.__name = name
        return self

    def __repr__(self):
        return self.__name

class CombineTwoParsers(TokenParser):
    """
    This class is used to combine two token parsers using the LSHIFT(<<)
    operator. Both parsers should succeed consuming tokens from the
    token list in order for the whole combination to succeed.
    If operation is applied over a sequence of parsers, the result will be
    parsers that are enclosed in each other:
    (parser1, (parser2, (parser3, ..., (parser4))). The structure of the
    resulting AST will correspond to this configuration.

    It can be applied to just any other parser:

        parser = parser1 << parser2 << parser3
    """

    def __init__(self, first, second):
        TokenParser.__init__(self)
        self.first = first
        self.second = second

    def __call__(self, tokens, pos):
        first_res = self.first(tokens, pos)
        if first_res:
            second_res = self.second(tokens, first_res.get_position())
            if second_res:
                if first_res.is_empty() or first_res.is_ignore():
                    first_res.add_instance(self)
                    return second_res
                if second_res.is_empty() or second_res.is_ignore():
                    # we are ignoring second result, update next tokens
                    # position in first result
                    first_res.set_position(second_res.get_position())
                    first_res.add_instance(self)
                    return first_res
                return ParserResult((first_res, second_res), \
                    second_res.get_position(), self)
        return None

class CombineManyParsers(TokenParser):
    """
    This class is used to combine many token parsers using the AND(&)
    operator into a single TokenParser. All parsers should succeed
    consuming tokens from the token list in order for the whole
    combination to be considered succeessful.
    The structure of the resulting AST will correspond to this
    configuration.

    It can be applied to just any other parser:

        parser = parser1 & parser2 & parser3
    """

    def __init__(self, first, second):
        TokenParser.__init__(self)
        self.parsers = []
        if isinstance(first, CombineManyParsers):
            self.parsers = self.parsers + first.parsers
        else:
            self.parsers.append(first)
        if isinstance(second, CombineManyParsers):
            self.parsers = self.parsers + second.parsers
        else:
            self.parsers.append(second)

    def __call__(self, tokens, pos):
        result = ()
        for parser in self.parsers:
            res = parser(tokens, pos)
            if not res:
                return None
            pos = res.get_position()
            if res.is_empty() or res.is_ignore():
                continue
            if res.unpack():
                res_token = res.get_token()
                if isinstance(res_token, tuple):
                    result = result + res_token
                else:
                    result = result + (res_token,)
            else:
                result = result + (res,)
        return ParserResult(result, pos, self)

class SelectParser(TokenParser):
    """
    This parser is used to combine two token parsers using the OR(|)
    bitwise operator. One of the parsers should succeed consuming a token
    from the token list in order for the whole combination to succeed.
    It can be applied to just any other parser:

        parser = parser1 | parser2 | parser3
    """

    def __init__(self, first, second):
        TokenParser.__init__(self)
        self.first = first
        self.second = second

    def __call__(self, tokens, pos):
        res = self.first(tokens, pos)
        if res:
            res.add_instance(self)
            return res
        res = self.second(tokens, pos)
        if res:
            res.add_instance(self)
            return res
        return None

class KeywordParser(TokenParser):
    """
    This parser consumes a Keyword from the grammar.

        IF = Keyword(r'if')

        parser = KeywordParser(IF) & parser1
    """

    def __init__(self, token):
        TokenParser.__init__(self)
        if not isinstance(token, Keyword):
            raise TypeError("argument '%s' must be a Keyword" % token)
        self.__token = token

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        (token, instance) = tokens[pos].get_data()
        if instance == self.__token:
            tokens[pos].touch()
            return ParserResult(token, pos + 1, instance)
        return None

class OperatorParser(TokenParser):
    """
    This parser consumes an Operator from the grammar.

        ASSIGNMENT = Operator(r'=')

        parser = OperatorParser(ASSIGNMENT) & parser1
    """

    def __init__(self, token):
        TokenParser.__init__(self)
        if not isinstance(token, Operator):
            raise TypeError("argument '%s' must be an Operator" % token)
        self.__token = token

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        (token, instance) = tokens[pos].get_data()
        if instance == self.__token:
            tokens[pos].touch()
            return ParserResult(token, pos + 1, instance)
        return None

class SymbolsParser(TokenParser):
    """
    This parser consumes Symbols from the grammar.

        IDENTIFIER = Symbol(r'[a-zA-Z_]*')

        parser = SymbolsParser(IDENTIFIER) & parser1
    """

    def __init__(self, instance):
        TokenParser.__init__(self)
        if not isinstance(instance, Symbols):
            raise TypeError("argument '%s' must be a Symbol" % instance)
        self.__instance = instance

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        (token, instance) = tokens[pos].get_data()
        if instance == self.__instance:
            tokens[pos].touch()
            return ParserResult(token, pos + 1, instance)
        return None

class Optional(TokenParser):
    """
    This parser marks a TokenParser to be optional. This means
    that the parser will not fail even if it does not find a token
    to consume. It can be applied to any other parser:

        parser = Optional(parser1) & parser2
    """

    def __init__(self, parser):
        TokenParser.__init__(self)
        self.__parser = parser

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        result = self.__parser(tokens, pos)
        if result:
            result.add_instance(self)
            return result
        return ParserResult(None, pos, self, True)

class ZeroOrMore(TokenParser):
    """
    This Parser marks a TokenParser to be optional and appear multiple
    times. The parser will be applied multiple times until it fails to
    consume token. It can be applied to any other parser:

        parser = parser1 & ZeroOrMore(parser2 | parser3)
    """

    def __init__(self, parser):
        TokenParser.__init__(self)
        self.__parser = parser

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        results = None
        while pos < len(tokens):
            result = self.__parser(tokens, pos)
            if not result:
                break
            result.add_instance(self)
            pos = result.get_position()
            if not results:
                results = result
            else:
                if isinstance(results, tuple):
                    results = results + (result,)
                else:
                    results = (results, result)
        if results and isinstance(results, ParserResult):
            return results
        return ParserResult(results, pos, self, True)

class Repeat(TokenParser):
    """
    This parser allows a TokenParser to appear multiple times. The parser
    will be applied multiple times until it fails to consume token.
    It can be applied to just any other parser:

        parser = Repeat(parser1) & parser2
    """

    def __init__(self, parser):
        TokenParser.__init__(self)
        self.__parser = parser

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        results = None
        while pos < len(tokens):
            result = self.__parser(tokens, pos)
            if not result:
                break
            result.add_instance(self)
            pos = result.get_position()
            if not results:
                results = result
            else:
                if isinstance(results, tuple):
                    results = results + (result,)
                else:
                    results = (results, result)
        if not results:
            return None
        if isinstance(results, ParserResult):
            return results
        return ParserResult(results, pos, self, True)

class AllTokensConsumed(TokenParser):
    """
    This parser makes sure that all input tokens are consumed.
    A parser of this type should be used when parsing the whole program
    is desired. Internally it uses CheckErrors parser as a top level
    parser so that parse errors are detected and reported.
    Raises ParseException exception

        parser = AllTokensConsumed(parser1 & (parser2 | parser3))
    """
    class __AllTokensConsumed(TokenParser):
        def __init__(self, parser):
            TokenParser.__init__(self)
            self.__parser = parser

        def __call__(self, tokens, pos):
            if pos >= len(tokens):
                return None
            result = self.__parser(tokens, pos)
            if not result:
                return None
            result.add_instance(self)
            pos = result.get_position()
            if pos != len(tokens):
                return None
            return result

    def __init__(self, parser):
        TokenParser.__init__(self)
        self.__parser = CheckErrors (self.__AllTokensConsumed(parser))

    def __call__(self, tokens, pos):
        result = self.__parser(tokens, pos)
        if result:
            result.add_instance(self)
        return result

class RecursiveParser(TokenParser):
    """
    This parser is used for parsing recursive grammars. It can be applied
    to just any other parser:

        parser = RecursiveParser()
        parser += parser1 + parser

    When it should be used:

    ifstmt = if + ( + cond + ) + { + ifstmt  + }

    Or:

    expr = a | expr, a
        a, a, a, a, a

    It is also useful when a parser uses or depends on another parser which
    is not defined yet.
    """

    def __init__(self):
        TokenParser.__init__(self)
        self.__parser = None

    def __iadd__(self, right):
        self.__parser = right
        return self

    def __call__(self, tokens, pos):
        if pos >= len(tokens):
            return None
        if not self.__parser:
            raise ValueError("Parser not set with recursive_parser += " \
                "another_parser")
        result = self.__parser(tokens, pos)
        if result:
            result.add_instance(self)
        return result

class CustomizeResult(TokenParser):
    """
    This parser is used for customizing nodes and building custom AST's.
    It can be applied to just any parser:
        parser = parser1 & CustomizedResult(parser2 & parser3)

    You can get ParserResult's node in the callback by calling
    parser.get_token(), reorganize it and update the result by calling
    parser.set_token().

    Example:
        result = ('1', '+', '2', ('+', 3))

    in the callback:
        (lo, op, ro, rest) = result
        rest_token = rest.get_token()
        (op2, ro2) = rest_token
        rest.set_token((op2, ro2, ro))
        result.set_token(op, lo, rest)

    result will look like that now:
        result = ('+', '1', ('+', 3, 2))
    """
    def __init__(self, parser, customize_func=None):
        TokenParser.__init__(self)
        self.__func = customize_func
        if not self.__func:
            self.__func = __default_customize
        self.__parser = parser

    def __default_customize(self, result):
        return result

    def __call__(self, tokens, pos):
        result = self.__parser(tokens, pos)
        if not result:
            return result
        result = self.__func(result)
        if result:
            result.add_instance(self)
        return result

class CheckErrors(TokenParser):
    """
    Use this parser as a top level parser in order to get relevant information
    (row, column, error message) regarding parse errors. Calling this parser
    will result in a ParseException exception if parsing fails.

        parser = CheckErrors(parser1 & parser2 & parser3)
    """

    def __init__(self, parser):
        TokenParser.__init__(self)
        self.__parser = parser

    def __call__(self, tokens, pos):
        result = self.__parser(tokens, pos)
        if not result:
            for i in range(0, len(tokens) - 1):
                if not tokens[i].is_touched():
                    raise ParseException(tokens[i].get_row(), \
                        tokens[i].get_column(), \
                        "Unknown symbol: %s" % tokens[i].get_token())
        return result


class ParseTests(unittest.TestCase):
    """ Follow the unit tests for the Lexer and the Parser """

    def __init__(self, *args, **kwargs):

        # define all tokens in the language
        self.FOR = Keyword(r'for')
        self.RETURN = Keyword(r'return')
        self.IF = Keyword(r'if')

	self.COMMA = Operator(r',')
	self.SEMICOLON = Operator(r';')
	self.ASSIGNMENT = Operator(r'=')
	self.LBRACKET = Operator(r'(')
	self.RBRACKET = Operator(r')')
	self.LBRACE = Operator(r'{')
	self.RBRACE = Operator(r'}')
	self.AND = Operator(r'&')
	self.POINTER = Operator('*')
	self.PP = Operator(r'++')
	self.LE = Operator(r'<=')

	self.IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
	self.INT_IDENTIFIER = Symbols(r'[0-9]*')
	self.STRING_IDENTIFIER = Symbols(r'\".*\"')

	self.CPP_STYLE_COMMENT = Ignore(r'//.*\n')
	self.MACROS = Ignore(r'\#.*\n')
	self.IGNORE_CHARS = Ignore(r'[ \t\v\f]+')

        # group tokens into sub-groups
        self.IGNORES = self.CPP_STYLE_COMMENT & self.MACROS & self.IGNORE_CHARS

        self.KEYWORDS = self.FOR & self.RETURN & self.IF

        self.OPERATORS = self.COMMA & self.SEMICOLON & self.ASSIGNMENT & \
            self.LBRACKET & self.RBRACKET & self.LBRACE & self.RBRACE & \
            self.AND & self.POINTER & self.PP & self.LE

        self.IDENTIFIERS = self.IDENTIFIER & self.INT_IDENTIFIER & \
            self.STRING_IDENTIFIER

        # join all token sub-groups
        self.TOKENS = self.KEYWORDS & self.OPERATORS & self.IDENTIFIERS & \
            self.IGNORES

        super(ParseTests, self).__init__(*args, **kwargs)

    def __checkEntry(self, expected_token, expected_instance, token_data):
        (token, instance) = token_data.get_data()
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
        self.__checkEntry(";", self.SEMICOLON, tokens[9])
        self.__checkEntry("long", self.IDENTIFIER, tokens[10])
        self.__checkEntry("result", self.IDENTIFIER, tokens[11])
        self.__checkEntry("=", self.ASSIGNMENT, tokens[12])
        self.__checkEntry("1", self.INT_IDENTIFIER, tokens[13])
        self.__checkEntry(";", self.SEMICOLON, tokens[14])
        self.__checkEntry(";", self.SEMICOLON, tokens[33])
        self.__checkEntry("return", self.RETURN, tokens[34])
        self.__checkEntry("result", self.IDENTIFIER, tokens[35])
        self.__checkEntry(";", self.SEMICOLON, tokens[36])
        self.__checkEntry("}", self.RBRACE, tokens[37])

    def testParser(self):

        # our source code
        source = r"""

        return 5;

        """

        IgnoreTokensInAST(self.SEMICOLON & self.LBRACE & self.RBRACE & \
            self.RBRACKET & self.LBRACKET)

        # obtain a list of all tokens present in the source
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(source)

        parser = AllTokensConsumed(KeywordParser(self.RETURN) & \
            SymbolsParser(self.INT_IDENTIFIER) & OperatorParser(self.SEMICOLON))
        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)

        parser = (KeywordParser(self.RETURN) | KeywordParser(self.FOR)) & \
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
              return;
            }
          }
        }

        """

        IgnoreTokensInAST(self.SEMICOLON & self.LBRACE & self.RBRACE & \
            self.RBRACKET & self.LBRACKET)

        # obtain a list of all tokens present in the source
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(source)

        #print tokens

        parser = RecursiveParser()
        parser += KeywordParser(self.IF) & OperatorParser(self.LBRACKET) & \
            SymbolsParser(self.INT_IDENTIFIER) & OperatorParser(self.RBRACKET) & \
            OperatorParser(self.LBRACE) & Optional(parser) & \
            Optional(KeywordParser(self.RETURN) & OperatorParser(self.SEMICOLON)) & \
            OperatorParser(self.RBRACE)
        
        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)
        #result.pretty_print()

    def testRecursiveParser2(self):

        # our source code
        source = r"""

        a, a, a, a =

        """

        IgnoreTokensInAST(self.SEMICOLON & self.LBRACE & self.RBRACE & \
            self.RBRACKET & self.LBRACKET)

        # obtain a list of all tokens present in the source
        lexer = Lexer(self.TOKENS)
        tokens = lexer.parseTokens(source)

        #print tokens

        assignment = OperatorParser(self.ASSIGNMENT)

        recursive = RecursiveParser()
        recursive += Repeat (OperatorParser(self.COMMA) & \
            recursive | SymbolsParser(self.IDENTIFIER))

        parser = AllTokensConsumed(recursive & assignment)

        result = parser(tokens, 0)
        self.assertTrue(result)
        #print(result)

def main():
    unittest.main()

if __name__ == '__main__':
    main()
