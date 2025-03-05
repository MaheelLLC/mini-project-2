from sly import Lexer, Parser

class DLangLexer(Lexer):
    tokens = {
        ID, NUMBER, PLUS, MINUS, TIMES, DIVIDE, ASSIGN,
        LPAREN, RPAREN, WHILE, IF, ELSE, INT, NOTHING, DOUBLE,
        BOOL, STRING, CLASS, INTERFACE, NULL, THIS, EXTENDS,
        IMPLEMENTS, FOR, RETURN, BREAK, NEW, ARRAYINSTANCE, 
        OUTPUT, INPUTINT, INPUTLINE, LT, LE, GT, GE, NE, EQ, 
        MODULUS, AND, OR, NOT, LSQUAREB, RSQUAREB, LCURLYB, 
        RCURLYB, SEMICOLON, COMMA, PERIOD, TRUE, FALSE, 
        COMMENT, COMMENT_BLOCK
    }

    ignore = ' \t\n'

    ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
    ID['if'] = IF
    ID['else'] = ELSE
    ID['while'] = WHILE
    ID['int'] = INT
    ID['nothing'] = NOTHING
    ID['double'] = DOUBLE
    ID['bool'] = BOOL
    ID['string'] = STRING
    ID['class'] = CLASS
    ID['interface'] = INTERFACE
    ID['null'] = NULL
    ID['this'] = THIS
    ID['extends'] = EXTENDS
    ID['implements'] = IMPLEMENTS
    ID['for'] = FOR
    ID['return'] = RETURN
    ID['break'] = BREAK
    ID['new'] = NEW
    ID['ArrayInstance'] = ARRAYINSTANCE
    ID['Output'] = OUTPUT
    ID['InputInt'] = INPUTINT
    ID['InputLine'] = INPUTLINE
    ID['True'] = TRUE
    ID['False'] = FALSE

    @_(r'"([^"\n]|\\.)*"')
    def STRING(self, t):
        return t

    # Updated regex for DOUBLE to handle scientific notation
    @_(r'\d+(\.\d*)?([eE][+-]?\d+)?|\.\d+([eE][+-]?\d+)?')
    def DOUBLE(self, t):
        if t.value.startswith('.'):
            print(f"Error: invalidDouble {t.value}")  # Error handling for numbers starting with a dot
            return None  # Skip invalid token
        return t

    NUMBER = r'\d+'
    
    COMMENT = r'//.*'
    COMMENT_BLOCK = r'/\*([^*]|\*+[^*/])*\*+/'
    
    PLUS = r'\+'
    MINUS = r'-'
    TIMES = r'\*'
    DIVIDE = r'/'
    ASSIGN = r'='
    LPAREN = r'\('
    RPAREN = r'\)'
    LSQUAREB = r'\['
    RSQUAREB = r'\]'
    LCURLYB = r'\{' 
    RCURLYB = r'\}'
    LE = r'<='
    LT = r'<'
    GE = r'>='
    GT = r'>'
    NE = r'!='
    EQ = r'=='
    MODULUS = r'%'
    AND = r'&&'
    OR = r'\|\|'
    NOT = r'!'
    SEMICOLON = r';'
    COMMA = r','
    PERIOD = r'\.'

    def error(self, t):
        print(f"Error: invalidString {t.value}")
        self.index += 1

class DLangParser(Parser):
    # grab the tokens from DLangLexer
    tokens = DLangLexer.tokens
    
    # initialize it with a symbol table
    def __init__(self):
        # the symbol table can store variables like NAME in the example
        self.symbol_table = {}
    
    # the decorator is the right hand side of the grammar rule
    # the function name is the left hand side of the grammar rule
    # the first grammar rule (the start symbol)
    @_('DeclPlus')
    def Program(self, p):
        # if we reach this function, we have a valid program
        print("Parsing completed successfully!")
        # the return value is the same as the decorator
        return p.DeclPlus
    
    