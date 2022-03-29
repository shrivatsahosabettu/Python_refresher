from sly import Lexer

class CalcLexer(Lexer):
    # Set of token names. This is always required
    tokens = {ID, NUMBER, WHILE, IF, ELSE, PRINT,
              PLUS, MINUS, TIMES, DIVIDE, ASSIGN,
              EQ, LT, LE, GT, GE, NE, LPAREN, RPAREN}

    literals = { '(', ')', '{', '}', ';'}

    # String containing ignored characters between tokens
    ignore = ' \t'

    # Other ignored patterns
    ignore_comment = r'\#.*'
    ignore_newline = r'\n+'

    # Regular expression rules for tokens
    ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
    NUMBER = r'\d+'
    PLUS = r'\+'
    MINUS = r'-'
    TIMES = r'\*'
    DIVIDE = r'/'
    EQ = r'=='
    ASSIGN = r'='
    LPAREN = r'\('
    RPAREN = r'\)'
    LE = r'<='
    LT = r'<'
    GE = r'>='
    GT = r'>'
    NE = r'!='

    @_(r'\d+')
    def NUMBER(self, t):
        t.value = int(t.value)
        return t

    # Identifiers and keywords
    ID['if'] = IF
    ID['else'] = ELSE
    ID['while'] = WHILE
    ID['print'] = PRINT

    # Line number tracking
    @_(r'\n+')
    def ignore_newline(self, t):
        self.lineno += t.value.count('\n')

    def error(self, t):
        print('Line %d: Bad character %r' % (self.lineno, t.value[0]))
        self.index += 1


if __name__ == '__main__':
    data = '''
# Counting
x = 0;
while (x < 10) {
    print x:
    x = x + 1;
}
'''
    lexer = CalcLexer()
    for tok in lexer.tokenize(data):
        print(tok)

