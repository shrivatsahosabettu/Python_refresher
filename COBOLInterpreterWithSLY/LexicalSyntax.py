from sly import Lexer


class CobLexer(Lexer):

    tokens = {COBOL_WORD, ALPHA_USER_DEFINED_WORDS, COBOL_WORD, INTEGER, LEVEL_NUMBER,
              DISPLAY, STOP, RUN, TEXT}

    # Literal characters to operator precedence override
    literals = {'(', ')','.',"'",'"'}


    # String containing ignored characters between tokens
    ignore = ' \t'

    # Other ignored patterns
    ignore_newline = r'\n+'

    '''
    A COBOL word is a character-string of not more than 30 characters which
    forms a user-defined word, a system-name, or a reserved word. Except for
    arithmetic operators and relation characters, each character of a   COBOL
    word is selected from the set of letters, digits, and the hyphen; the hyphen 
    cannot appear as the first or last character in such words. Each lowercase 
    letter is considered to be equivalent to its corresponding uppercase letter.
    '''

    COBOL_WORD = r'[A-Za-z0-9]+([\-]+[A-Za-z0-9]+)*'

    # User defined words
    ALPHA_USER_DEFINED_WORDS = r'([0-9]+[\-]*)*[0-9]*[A-Za-z][A-Za-z0-9]*([\-]+[A-Za-z0-9]+)*'

    TEXT = r'~("\n" | "\r")'

    '''
    The word integer appearing in a format represents a numeric literal of nonzero 
    value that contains no sign and no decimal point; any other restrictions are
    included with the description of the format.
    It is actually questionable, if we should forbid nonzero values at the liexical level.
    Moreover, for some uses of integer, zero is actually allowed in contrast to the above rule.
    '''

    INTEGER = r'[0]*[1-9][0-9]*'
    @_(r'[0]*[1-9][0-9]*')
    def INTEGER(self, t):
        t.value = int(t.value)
        return t
    '''
    level-numbers: 01-49, 66, 77, 88
    Each word must be a 1-digit or 2-digit integer
    '''

    LEVEL_NUMBER = r'([0]?[1-9]|[1-4][0-9]|"66"|"77"|"88")'

    # Identifiers and keywords
    COBOL_WORD['DISPLAY', 'display'] = DISPLAY
    COBOL_WORD['STOP', 'stop'] = STOP
    COBOL_WORD['RUN', 'run'] = RUN

    @_(r'\n+')
    def ignore_newline(self, t):
        self.lineno += t.value.count('\n')

    def error(self, t):
        print('Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1


if __name__ == '__main__':
    data = '''
       ID-DIVISION. 
       PROGRAM-ID. TEST01.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
'''
    lexer = CobLexer()
    for tok in lexer.tokenize(data):
        print(tok)