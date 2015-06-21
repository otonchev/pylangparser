# Example usage of the Parser: parse Food Recipes.
#
# Copyright, 2015, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("..")

from pylangparser import *

QTY = Symbols(r'\d+')
QTY_HALF = Symbols(r'(\d+ ){0,1}1/2')
QTYS = QTY_HALF & QTY

ONION = Symbols(r'onion')
ROAST = Symbols(r'roast')
SOY_SAUCE = Symbols(r'soy sauce')
STALK_CELERY = Symbols(r'stalk celery')
WATER = Symbols(r'water')
YOGURT = Symbols(r'yogurt')

FAT = Symbols(r'\d+%')

INGREDIENT = \
    ONION & \
    ROAST & \
    SOY_SAUCE & \
    STALK_CELERY & \
    WATER & \
    YOGURT

RANDOM_WORD = Symbols(r'[a-zA-Z]+')

LBS = Symbols(r'lbs')
TABLESPOON = Symbols(r'tablespoon')
TABLESPOONS = Symbols(r'tablespoons')
TEASPOON = Symbols(r'teaspoon')
TEASPOONS = Symbols(r'teaspoons')
QUART = Symbols(r'quart')
QUARTS = Symbols(r'quarts')
DASH = Symbols(r'dash')
DASHES = Symbols(r'dashes')
CUP = Symbols(r'cup')
CUPS = Symbols(r'cups')

METRICS = \
    LBS & \
    TABLESPOON & \
    TABLESPOONS & \
    TEASPOON & \
    TEASPOONS & \
    QUART & \
    QUARTS & \
    DASH & \
    DASHES & \
    CUP & \
    CUPS

IGNORE_CHARS = Ignore(r'[ \n,.-]+')

IGNORES = IGNORE_CHARS

# order is important, first token that is matches will be considered
TOKENS = FAT & QTYS & METRICS & IGNORES & INGREDIENT & RANDOM_WORD

food_recipe = """

3 lbs chuck roast
1 quart water
1 1/2 quarts water
1 onion, chopped
1 stalk celery, chopped
2 tablespoons soy sauce
1/2 cup 2% Greek yogurt

"""

ingredient = \
    SymbolsParser(ONION) | \
    SymbolsParser(ROAST) | \
    SymbolsParser(SOY_SAUCE) | \
    SymbolsParser(STALK_CELERY) | \
    SymbolsParser(WATER) | \
    SymbolsParser(YOGURT)

measure = \
    SymbolsParser(CUP) | \
    SymbolsParser(CUPS) | \
    SymbolsParser(LBS) | \
    SymbolsParser(QUART) | \
    SymbolsParser(QUARTS) | \
    SymbolsParser(TABLESPOON) | \
    SymbolsParser(TABLESPOONS)

qty = \
    SymbolsParser(QTY_HALF) | \
    SymbolsParser(QTY)

fat = SymbolsParser(FAT)

ingredient = qty << ZeroOrMore(measure) << Repeat(fat | ingredient | IgnoreResult(SymbolsParser(RANDOM_WORD)))

recipe = ZeroOrMore(ingredient)

# extract tokens
lexer = Lexer(TOKENS)
tokens = lexer.parseTokens(food_recipe, False)
print(tokens)

parser = AllTokensConsumed(recipe)

# generating AST
ast = parser(tokens, 0)
print("\nast:")
ast.pretty_print()
