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
    
    # define what Decl+ means
    @_('DeclPlus Decl')
    def DeclPlus(self, p):
        # recursion allows for infinite Decl's
        # adding two objects together is easier as an array
        return p.DeclPlus + [p.Decl]
    
    # the base case of the recursion above
    @_('Decl')
    def DeclPlus(self, p):
        # Decl+ can now either return Decl or [Decl, Decl, ...]
        return [p.Decl]
    
    @_('VariableDecl')
    def Decl(self, p):
        # Decl produces a VariableDecl
        return p.VariableDecl
    
    @_('FunctionDecl')
    def Decl(self, p):
        # Decl also produces a FunctionDecl
        return p.FunctionDecl
    
    # VariableDecl -> Variable;
    # ; is a SEMICOLON in the lexical analyzer
    @_('Variable SEMICOLON')
    def VariableDecl(self, p):
        print("Found VariableDecl")
        return p.Variable
    
    # Variable -> Type ident
    @_('Type ID')
    def Variable(self, p):
        # p.Type is the variable's type
        # p.ID is the variable's name from the lexer
        return (p.Type, p.ID)
    
    # Type -> int
    @_('INT')
    def Type(self, p):
        return p.INT
    
    # Type -> double
    @_('DOUBLE')
    def Type(self, p):
        return p.DOUBLE
    
    # Type -> bool
    @_('BOOL')
    def Type(self, p):
        return p.BOOL
    
    # Type -> string
    @_('STRING')
    def Type(self, p):
        return p.STRING
    
    # Type ident ( Formals ) StmtBlock 
    @_('Type ID LPAREN Formals RPAREN StmtBlock')
    def FunctionDecl(self, p):
        return (p.Type, p.ID, p.LPAREN, p.Formals, p.RPAREN, p.StmtBlock)
    
    # nothing ident ( Formals ) StmtBlock
    @_('NOTHING ID LPAREN Formals RPAREN StmtBlock')
    def FunctionDecl(self, p):
        return (p.NOTHING, p.ID, p.LPAREN, p.Formals, p.RPAREN, p.StmtBlock)
    
    # Formals -> Variable+,
    @_('VariablePlus COMMA')
    def Formals(self, p):
        return (p.VariablePlus, p.COMMA)
    
    # Variable+
    @_('VariablePlus Variable')
    def VariablePlus(self, p):
        # recursively collect all variable declarations into a list
        return p.VariablePlus + [p.Variable]
    
    # the base of recursion above
    @_('Variable')
    def VariablePlus(self, p):
        # Variable+ can now either be Variable or [Variable, Variable, ...]
        return p.Variable
    
    # returns an emply list if there are no formals
    @_('')
    def Formals(self, p):
        return []
    
    @_('LCURLYB VariableDeclList StmtList RCURLYB')
    def StmtBlock(self, p):
        return (p.LCURLYB, p.VariableDeclList , p.StmtList , p.RCURLYB)

    # VariableDecl*
    # Recursively collects variable declarations into a list.
    @_('VariableDeclList VariableDecl')
    def VariableDeclList(self, p):
        return p.VariableDeclList + [p.VariableDecl]
    
    # if empty return an empty list
    @_('')
    def VariableDeclList(self, p):
        return []
    
    # Stmt*
    # Recursively collects statements into a list.
    @_('StmtList Stmt')
    def StmtList(self, p):
        return p.StmtList + [p.Stmt]
    
    # if empty, return an empty list
    @_('')
    def StmtList(self, p):
        return []
    
    # Call -> ident (Actuals)
    @_('ID LPAREN Actuals RPAREN')
    def Call(self, p):
        return (p.ID, p.LPAREN, p.Actuals, p.RPAREN)

    # Actuals -> Expr+, 
    @_('ExprPlus COMMA')
    def Actuals(self, p):
        return (p.ExprPlus, p.COMMA)

    # Expr+
    @_('ExprPlus Expr')
    def ExprPlus(self, p):
        # to represent any number of Expr, we use recursion
        return p.ExprPlus + [p.Expr]

    # the base case for Expr+ (a single Expr)
    @_('Expr')
    def ExprPlus(self, p):
        # Expr+ can now either be Expr or [Expr, Expr, ...]
        return [p.Expr]

    # Actuals -> empty
    @_('empty')
    def Actuals(self, p):
        return []

    # Constant -> intConstant
    @_('INT Constant')
    def Constant(self, p):
        return (p.INT, p.Constant)

    # Constant -> doubleConstant
    @_('DOUBLE Constant')
    def Constant(self, p):
        return (p.DOUBLE, p.Constant)

    # Constant -> boolConstant
    @_('BOOL Constant')
    def Constant(self, p):
        return (p.BOOL, p.Constant)

    # Constant -> stringConstant
    @_('STRING Constant')
    def Constant(self, p):
        return (p.STRING, p.Constant)

    # Constant -> null
    @_('NULL')
    def Constant(self, p):
        return p.NULL



    
if __name__ == '__main__':
    # store the test file name
    filename = 'test-parser.dlang'
    
    # open and read the file 
    # with statement automatically closes the file
    with open(filename, 'r') as file:
        data = file.read()
        
    # create a lexer and parser
    lexer = DLangLexer()
    parser = DLangParser()
    
    # parse the file after tokenizing the contents
    result = parser.parse(lexer.tokenize(data))
    
    # show the result
    print(result)