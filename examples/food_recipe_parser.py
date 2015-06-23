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

ONION = Symbols(r'onion', ignorecase=True)
ROAST = Symbols(r'roast', ignorecase=True)
SOY_SAUCE = Symbols(r'soy sauce', ignorecase=True)
STALK_CELERY = Symbols(r'stalk celery', ignorecase=True)
WATER = Symbols(r'water', ignorecase=True)
YOGURT = Symbols(r'yogurt', ignorecase=True)

FAT = Symbols(r'\d+%')

INGREDIENT = \
    ONION & \
    ROAST & \
    SOY_SAUCE & \
    STALK_CELERY & \
    WATER & \
    YOGURT

RANDOM_WORD = Symbols(r'[a-zA-Z]+')

LBS = Symbols(r'lbs', ignorecase=True)
TABLESPOON = Symbols(r'tablespoon', ignorecase=True)
TABLESPOONS = Symbols(r'tablespoons', ignorecase=True)
TEASPOON = Symbols(r'teaspoon', ignorecase=True)
TEASPOONS = Symbols(r'teaspoons', ignorecase=True)
QUART = Symbols(r'quart', ignorecase=True)
QUARTS = Symbols(r'quarts', ignorecase=True)
DASH = Symbols(r'dash', ignorecase=True)
DASHES = Symbols(r'dashes', ignorecase=True)
CUP = Symbols(r'cup', ignorecase=True)
CUPS = Symbols(r'cups', ignorecase=True)

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

# order is important, first token that matches will be considered
TOKENS = FAT & QTYS & METRICS & IGNORES & INGREDIENT & RANDOM_WORD

food_recipe = """

3 lbs chuck roast
1 quart water
1 1/2 quarts water
1 onion, chopped
1 stalk celery, chopped
2 tablespoons soy sauce 1/2 cup 2% Greek Yogurt

"""

product = \
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

ingredient = qty << ZeroOrMore(measure) << \
    Repeat(fat | product | IgnoreResult(SymbolsParser(RANDOM_WORD)))

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

for group in ast:
    if group.check_parser(ingredient):
        print("\ningredient:")
        for sub_group in group:
            if sub_group.check_parser(qty):
                print("qty: %s" % sub_group.get_token())
            if sub_group.check_parser(measure):
                print("measure: %s" % sub_group.get_token())
            if sub_group.check_parser(product):
                print("product: %s" % sub_group.get_token())
            if sub_group.check_parser(fat):
                print("fat level: %s" % sub_group.get_token())
