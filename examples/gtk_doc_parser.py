# Example usage of the Parser. The test parses a Gtk-Doc style
# comment section and validates the grammar. It explicitely parses
# annotations.
#
# Copyright, 2014, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("..")

from pylangparser import *

TYPE = Keyword(r'\(type [A-Za-z\.]+\)')
ALLOW_NONE = Keyword(r'\(allow-none\)')
FLOATING = Keyword(r'\(transfer floating\)')
FULL = Keyword(r'\(transfer full\)')
NONE = Keyword(r'\(transfer none\)')
CONTAINER = Keyword(r'\(transfer container\)')
NULLABLE = Keyword(r'\(nullable\)')
OUT = Keyword(r'\(out\)')
RETURNS = Keyword(r'Returns')

KEYWORDS = TYPE & ALLOW_NONE & FULL & NONE & CONTAINER & FLOATING & NULLABLE & \
    OUT & RETURNS

COLON = Operator(r':')
AT = Operator(r'@')
COMMA = Ignore(r',')
DOT = Ignore(r'.')

OPERATORS = AT & COLON & COMMA & DOT

VARARGS = Symbols(r'\.\.\.')
IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
WORD = Symbols(r'[A-Za-z#%\(\)]+')
COMMENT_START = Symbols(r'/\*\*')
COMMENT_END = Symbols(r'\*/')

SYMBOLS = VARARGS & IDENTIFIER & WORD
COMMENT_SYMBOLS = COMMENT_START & COMMENT_END

COMMENT_LINE = Ignore(r'\*')
IGNORE_CHARS = Ignore(r'[ \t\v\f\n]+')

IGNORES = IGNORE_CHARS & COMMENT_LINE

# order is important, first token that is matches will be considered
# that is why it is important to put '*/' before '*', for example
TOKENS = COMMENT_SYMBOLS & KEYWORDS & SYMBOLS & OPERATORS & IGNORES

IgnoreTokensInAST(AT & COLON & DOT & COMMENT_START & COMMENT_END)

gtk_doc = """

/**
 * gst_element_get_parent:
 * @elem: a #GstElement to get the parent of.
 *
 * Get the parent of an element.
 *
 * Returns: (transfer full): the parent of an element.
 */

/**
 * gst_pad_new_from_template:
 * @templ: the pad template to use
 * @name: (allow-none) (nullable): the name of the pad
 * @...: variable arguments
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
 */

/**
 * gst_object_unref:
 * @object: (type Gst.Object): a #GstObject to unreference
 *
 * Decrements the reference count on @object.  If reference count hits
 * zero, destroy @object. This function does not take the lock
 * on @object as it relies on atomic refcounting.
 *
 * The unref method should never be called with the LOCK held since
 * this might deadlock the dispose function.
 */

"""

# extract tokens
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(gtk_doc, False)
print(tokens)

ignored_word = \
    IgnoreResult(SymbolsParser(WORD) | SymbolsParser(IDENTIFIER) | \
    (OperatorParser(AT) & SymbolsParser(IDENTIFIER)))
annotation = \
    KeywordParser(ALLOW_NONE) | KeywordParser(FLOATING) | \
    KeywordParser(NULLABLE) | KeywordParser(FULL) | KeywordParser(TYPE)
func_name = SymbolsParser(IDENTIFIER)
func = func_name & OperatorParser(COLON)
returns = \
    KeywordParser(RETURNS) << OperatorParser(COLON) << \
    Optional(ZeroOrMore(annotation) << OperatorParser(COLON))
arg = \
    OperatorParser(AT) << \
    (SymbolsParser(IDENTIFIER) | SymbolsParser(VARARGS)) << \
    OperatorParser(COLON) << \
    Optional(ZeroOrMore(annotation) << OperatorParser(COLON))
func_desc = ZeroOrMore(arg | returns | ignored_word)
description = \
    SymbolsParser(COMMENT_START) << \
    func << \
    func_desc << \
    SymbolsParser(COMMENT_END)

parser = AllTokensConsumed(ZeroOrMore(description))

# generating AST
ast = parser(tokens, 0)
print("\nast:")
ast.pretty_print()

# iterating AST
print("\nparsing...")

for function_block in ast:
    for function_desc in function_block:
        if function_desc.check_parser(func_name):
            print("func name:")
            print function_desc.get_token()
        if function_desc.check_parser(arg):
            print("arg:")
            index = 0
            for func_arg in function_desc:
                if index == 1:
                    print("annotations:")
                print func_arg.get_token()
                index += 1
        if function_desc.check_parser(returns):
            print("returns:")
            index = 0
            for func_ret in function_desc:
                if index > 0:
                    print func_ret.get_token()
                index += 1
