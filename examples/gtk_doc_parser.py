# Example usage of the Parser. The test parses a Gtk-Doc style
# comment section and validates the grammar. It explicitely parses
# annotations.
#
# Copyright, 2014, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("..")

from pylangparser import *

ALLOW_NONE = Keyword(r'\(allow-none\)')
FLOATING = Keyword(r'\(transfer floating\)')
NULLABLE = Keyword(r'\(nullable\)')
RETURNS = Keyword(r'Returns')

KEYWORDS = ALLOW_NONE & FLOATING & NULLABLE & RETURNS

COLON = Operator(r':')
AT = Operator(r'@')
COMMA = Ignore(r',')
DOT = Ignore(r'.')

OPERATORS = AT & COLON & COMMA & DOT

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
WORD = Symbols(r'[A-Za-z#%\(\)]+')

COMMENT_START = Symbols(r'/\*\*')
COMMENT_END = Symbols(r'\*/')
COMMENT_LINE = Ignore(r'\*')
IGNORE_CHARS = Ignore(r'[ \t\v\f\n]+')

IGNORES = IGNORE_CHARS & COMMENT_START & COMMENT_END & COMMENT_LINE

TOKENS = COMMENT_START & COMMENT_END & KEYWORDS & OPERATORS & IDENTIFIER & \
    WORD & IGNORES

IgnoreTokensInAST(AT & COLON & DOT & COMMENT_START & COMMENT_END)

gtk_doc = """

/**
 * gst_pad_new_from_template:
 * @templ: the pad template to use
 * @name: (allow-none): the name of the pad
 *
 * Creates a new pad with the given name from the given template.
 * If name is %NULL, a guaranteed unique name (across all pads)
 * will be assigned.
 * This function makes a copy of the name so you can safely free the name.
 *
 * Returns: (transfer floating) (nullable): a new #GstPad, or %NULL in
 * case of an error.
 */

/**
 * gst_pad_new_from_template:
 * @templ: the pad template to use
 * @name: (allow-none): the name of the pad
 *
 * Creates a new pad with the given name from the given template.
 * If name is %NULL, a guaranteed unique name (across all pads)
 * will be assigned.
 * This function makes a copy of the name so you can safely free the name.
 *
 * Returns: (transfer floating) (nullable): a new #GstPad, or %NULL in
 * case of an error.
 */

"""

lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(gtk_doc, False)
print(tokens)

ignored_words = \
    IgnoreResult(ZeroOrMore(SymbolsParser(WORD) | SymbolsParser(IDENTIFIER)))
annotation = \
    KeywordParser(ALLOW_NONE) | KeywordParser(FLOATING) | \
    KeywordParser(NULLABLE)
annotations = \
    ZeroOrMore(annotation) & OperatorParser(COLON)
func_name = \
    SymbolsParser(IDENTIFIER) & OperatorParser(COLON)
arg = \
    OperatorParser(AT) & SymbolsParser(IDENTIFIER) & OperatorParser(COLON) & \
    Optional(annotations) & ignored_words
returns = \
    KeywordParser(RETURNS) & OperatorParser(COLON) & \
    ZeroOrMore(annotation) & OperatorParser(COLON) & ignored_words

description = \
    SymbolsParser(COMMENT_START) & \
    func_name & \
    ZeroOrMore(arg) & \
    Optional(returns) & \
    SymbolsParser(COMMENT_END)
parser = AllTokensConsumed(ZeroOrMore(description))

ast = parser(tokens, 0)
ast.pretty_print()
