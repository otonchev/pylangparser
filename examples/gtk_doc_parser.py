# Example usage of the Parser. The test parses a Gtk-Doc style
# comment section and validates the grammar. It explicitely parses
# annotations.
#
# Copyright, 2014, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("..")

from pylangparser import *

ALLOW_NONE = Keyword(r'allow-none')
FLOATING = Keyword(r'transfer floating')
NULLABLE = Keyword(r'nullable')
RETURNS = Keyword(r'Returns')

KEYWORDS = ALLOW_NONE & FLOATING & NULLABLE & RETURNS

L_PAR = Operator(r'(')
R_PAR = Operator(r')')
COLON = Operator(r':')
AT = Operator(r'@')
COMMA = Operator(r',')
DOT = Operator(r'.')

OPERATORS = L_PAR & R_PAR & AT & COLON & COMMA & DOT

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
WORD = Symbols(r'[A-Za-z#%]+')

COMMENT_START = Ignore(r'/\*\*')
COMMENT_END = Ignore(r'\*/')
COMMENT_LINE = Ignore(r'\*')
IGNORE_CHARS = Ignore(r'[ \t\v\f\n]+')

IGNORES = IGNORE_CHARS & COMMENT_START & COMMENT_END & COMMENT_LINE

TOKENS = KEYWORDS & OPERATORS & IDENTIFIER & WORD & IGNORES

IgnoreTokensInAST(R_PAR & L_PAR & COLON & DOT)

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

"""

lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(gtk_doc, False)
print(tokens)

# actual grammar is under development
