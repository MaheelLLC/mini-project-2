from sly import Lexer, Parser

class DLangLexer(Lexer):
    # store the tokens as a set of token names
    tokens = {
        ID, INTCONST, PLUS, MINUS, TIMES, DIVIDE, ASSIGN,
        LPAREN, RPAREN, WHILE, IF, ELSE, INT, NOTHING, DOUBLE,
        BOOL, STRING, CLASS, INTERFACE, NULL, THIS, EXTENDS,
        IMPLEMENTS, FOR, RETURN, BREAK, NEW, ARRAYINSTANCE, 
        OUTPUT, INPUTINT, INPUTLINE, LT, LE, GT, GE, NE, EQ, 
        MODULUS, AND, OR, NOT, LSQUAREB, RSQUAREB, LCURLYB, 
        RCURLYB, SEMICOLON, COMMA, PERIOD, TRUE, FALSE, 
        COMMENT, COMMENT_BLOCK, DOUBLECONST, STRINGCONST
    }

    # ignore white space and tab space
    ignore = ' \t'
    # ignore single line comments (negate the single line comment if a new line
    # starts as well) Any character after // is ignored if it isn't a new line
    ignore_comment = r'//[^\n]*'
    # ignore multi line comments. [\s\S] matches to any whitespace and
    # non-whitespace character (in reality, this is just all characters in 
    # between /* */. The ? looks for the first time */ appears to find exactly
    # where the comment closes
    ignore_multiline_comment = r'/\*[\s\S]*?\*/'
    
    # track line numbers for syntax errors
    @_(r'\n+')
    def ignore_newline(self, t):
        # increment the line number for every \n
        self.lineno += t.value.count('\n')

    # this regular expression was given by the documentation for IDs
    ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
    ID['if'] = IF
    ID['else'] = ELSE
    ID['while'] = WHILE
    ID['int'] = INT
    ID['nothing'] = NOTHING
    ID['double'] = DOUBLE
    ID['bool'] = BOOL
    ID['string'] = STRING # keyword STRING actually means string
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

    # creates the regex for a literal string (ex. " string ")
    @_(r'"([^"\n]|\\.)*"')
    def STRINGCONST(self, t):
        return t

    # Updated regex for DOUBLE to handle scientific notation
    @_(r'\d+(\.\d*)?([eE][+-]?\d+)?|\.\d+([eE][+-]?\d+)?')
    def DOUBLECONST(self, t):
        if t.value.startswith('.'):
            print(f"Error: invalidDouble {t.value}")  # Error handling for numbers starting with a dot
            return None  # Skip invalid token
        return t
    # An integer is just a number with only digits
    INTCONST = r'\d+'
    
    # ignored as defined above
    COMMENT = r'//.*'
    COMMENT_BLOCK = r'/\*([^*]|\*+[^*/])*\*+/'
    
    # given by the documentation demo
    PLUS = r'\+'
    MINUS = r'-'
    TIMES = r'\*'
    DIVIDE = r'/'
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
    ASSIGN = r'='
    MODULUS = r'%'
    AND = r'&&'
    OR = r'\|\|'
    NOT = r'!'
    SEMICOLON = r';'
    COMMA = r','
    PERIOD = r'\.'
    
    # a function that initializes the symbol table.
    def initialize_symbol_table(self):
        # make the symbol table as a dictionary
        self.symbol_table = {}

    # whenever we reach a lexical error
    def error(self, t):
        # print out the token causing the error and its line number
        print(f"Error: invalidString {t.value} at line {self.lineno}")
        # move on to the next character to continue analysis
        self.index += 1

class DLangParser(Parser):
    # grab the tokens from DLangLexer
    tokens = DLangLexer.tokens
    
    # initialize it with a symbol table
    def __init__(self):
        # the symbol table can store variables like NAME in the example
        self.symbol_table = {}
    
    # due to many shift-reduce/reduce-reduce conflicts
    # we added a precedence tuple (syntax derived from github example)
    # since DLang had many C++ operators, we followed C++'s operator precedence
    # associativity (which side the expression is on) is also determined by C++ 
    # references
    # left means the expression is on the left side of the operator (ex. Expr AND...)
    # right means the expression is on the right side of the operator (ex. !Expr)
    # precedence in C++ is as follows
    # logical NOT and unary minus -> multiplication, division, and modulus ->
    # addition and subtraction -> relational operators -> equality operators ->
    # logical AND -> logical OR
    precedence = (
        ('left', OR),
        ('left', AND),
        ('left', EQ, NE),
        ('left', LT, LE, GT, GE),
        ('left', PLUS, MINUS),
        ('left', TIMES, DIVIDE, MODULUS),
        ('right', NOT, UMINUS),
    )
    
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
    
    # Decl -> VariableDecl
    @_('VariableDecl')
    def Decl(self, p):
        # Decl produces a VariableDecl
        return p.VariableDecl
    
    # Decl -> FunctionDecl
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
        print("Found Variable")
        # p.Type is the variable's type
        # p.ID is the variable's name from the lexer
        return (p.Type, p.ID)
    
    # Type -> int
    @_('INT')
    def Type(self, p):
        print("Found Type INT")
        return p.INT
    
    # Type -> double
    @_('DOUBLE')
    def Type(self, p):
        print("Found Type DOUBLE")
        return p.DOUBLE
    
    # Type -> bool
    @_('BOOL')
    def Type(self, p):
        print("Found Type BOOL")
        return p.BOOL
    
    # Type -> string
    @_('STRING')
    def Type(self, p):
        print("Found Type STRING")
        return p.STRING
    
    # Type ident ( Formals ) StmtBlock 
    @_('Type ID LPAREN Formals RPAREN StmtBlock')
    def FunctionDecl(self, p):
        print("Found FunctionDecl with Type")
        return (p.Type, p.ID, p.LPAREN, p.Formals, p.RPAREN, p.StmtBlock)
    
    # nothing ident ( Formals ) StmtBlock
    @_('NOTHING ID LPAREN Formals RPAREN StmtBlock')
    def FunctionDecl(self, p):
        print("Found FunctionDecl with NOTHING")
        return (p.NOTHING, p.ID, p.LPAREN, p.Formals, p.RPAREN, p.StmtBlock)
    
    # Formals -> Variable+
    @_('VariablePlus')
    def Formals(self, p):
        print("Found Formals with VariablePlus")
        return p.VariablePlus
    
    # Variable+ -> Variable+ , Variable
    @_('VariablePlus COMMA Variable')
    def VariablePlus(self, p):
        print("Found VariablePlus with VariablePlus, COMMA, and Variable")
        # recursively collect all variable declarations into a list
        return p.VariablePlus + [p.Variable]
    
    # the base case of the recursion above
    @_('Variable')
    def VariablePlus(self, p):
        print("Found VariablePlus with Variable")
        # Variable+ can now either be Variable or [Variable, Variable, ...]
        return [p.Variable]
    
    # returns an emply list if there are no formals
    @_('')
    def Formals(self, p):
        print("Found empty Formals")
        return []
    
    # StmtBlock -> { VariableDecl* Stmt* }
    @_('LCURLYB VariableDeclList StmtList RCURLYB')
    def StmtBlock(self, p):
        print("Found StmtBlock")
        return (p.LCURLYB, p.VariableDeclList , p.StmtList , p.RCURLYB)

    # VariableDecl*
    # Recursively collects variable declarations into a list.
    @_('VariableDeclList VariableDecl')
    def VariableDeclList(self, p):
        print("Found VariableDeclList with VariableDeclList and VariableDecl")
        return p.VariableDeclList + [p.VariableDecl]
    
    # if empty return an empty list
    @_('')
    def VariableDeclList(self, p):
        print("Found empty VariableDeclList")
        return []
    
    # Stmt*
    # Recursively collects statements into a list.
    @_('StmtList Stmt')
    def StmtList(self, p):
        print("Found StmtList with StmtList and Stmt")
        return p.StmtList + [p.Stmt]
    
    # if empty, return an empty list
    @_('')
    def StmtList(self, p):
        print("Found empty StmtList")
        return []
    
    # Stmt -> <Expr> ;
    @_('OptionalExpr SEMICOLON')
    def Stmt(self, p):
        print("Found Stmt with OptionalExpr and SEMICOLON")
        return (p.OptionalExpr, p.SEMICOLON)
    
    # <Expr> -> Expr (optional Expr can return 1 Expr)
    @_('Expr')
    def OptionalExpr(self, p):
        print("Found OptionalExpr with Expr")
        return p.Expr
    
    # <Expr> -> empty (optional Expr can also return 0 Expr)
    @_('')
    def OptionalExpr(self, p):
        print("Found empty OptionalExpr")
        return []
    
    # Stmt -> IfStmt
    @_('IfStmt')
    def Stmt(self, p):
        print("Found Stmt with IfStmt")
        return p.IfStmt
    
    # Stmt -> WhileStmt
    @_('WhileStmt')
    def Stmt(self, p):
        print("Found Stmt with WhileStmt")
        return p.WhileStmt
    
    # Stmt -> ForStmt
    @_('ForStmt')
    def Stmt(self, p):
        print("Found Stmt with ForStmt")
        return p.ForStmt
    
    # Stmt -> BreakStmt
    @_('BreakStmt')
    def Stmt(self, p):
        print("Found Stmt with BreakStmt")
        return p.BreakStmt
    
    # Stmt -> ReturnStmt
    @_('ReturnStmt')
    def Stmt(self, p):
        print("Found Stmt with ReturnStmt")
        return p.ReturnStmt
    
    # Stmt -> OutputStmt
    @_('OutputStmt')
    def Stmt(self, p):
        print("Found Stmt with OutputStmt")
        return p.OutputStmt
    
    # Stmt -> StmtBlock
    @_('StmtBlock')
    def Stmt(self, p):
        print("Found Stmt with StmtBlock")
        return p.StmtBlock
    
    # IfStmt -> if ( Expr ) Stmt <else Stmt>
    @_('IF LPAREN Expr RPAREN Stmt OptionalElseStmt')
    def IfStmt(self, p):
        print("Found IfStmt")
        return (p.IF, p.LPAREN, p.Expr, p.RPAREN, p.Stmt, p.OptionalElseStmt)
    
    # <else Stmt> -> else Stmt (optional else Stmt can return 1 "else Stmt")
    @_('ELSE Stmt')
    def OptionalElseStmt(self, p):
        print("Found OptionalElseStmt with ELSE and Stmt")
        return (p.ELSE, p.Stmt)
    
    # <else Stmt> -> empty (optional else Stmt can return 0 "else Stmt"s)
    @_('')
    def OptionalElseStmt(self, p):
        print("Found empty OptionalElseStmt")
        return []
    
    # WhileStmt -> while ( Expr ) Stmt
    @_('WHILE LPAREN Expr RPAREN Stmt')
    def WhileStmt(self, p):
        print("Found WhileStmt")
        return (p.WHILE, p.LPAREN, p.Expr, p.RPAREN, p.Stmt)
    
    # ForStmt -> for ( <Expr> ; Expr ; <Expr> ) Stmt
    @_('FOR LPAREN OptionalExpr SEMICOLON Expr SEMICOLON OptionalExpr RPAREN Stmt')
    def ForStmt(self, p):
        print("Found ForStmt")
        return (p.FOR, p.LPAREN, p.OptionalExpr0, p.SEMICOLON0, p.Expr, p.SEMICOLON1, p.OptionalExpr1, p.RPAREN, p.Stmt)
    
    # ReturnStmt -> return <Expr> ;
    @_('RETURN OptionalExpr SEMICOLON')
    def ReturnStmt(self, p):
        print("Found ReturnStmt")
        return (p.RETURN, p.OptionalExpr, p.SEMICOLON)
    
    # BreakStmt -> break ;
    @_('BREAK SEMICOLON')
    def BreakStmt(self, p):
        print("Found BreakStmt")
        return (p.BREAK, p.SEMICOLON)
    
    # OutputStmt -> Output ( Expr+ , ) ;
    @_('OUTPUT LPAREN ExprPlus RPAREN SEMICOLON')
    def OutputStmt(self, p):
        print("Found OutputStmt")
        return (p.OUTPUT, p.LPAREN, p.ExprPlus, p.RPAREN, p.SEMICOLON)
    
    # Expr+ -> Expr+ Expr
    @_('ExprPlus COMMA Expr')
    def ExprPlus(self, p):
        print("Found ExprPlus with ExprPlus, COMMA, and Expr")
        # to represent any number of Expr, we use recursion
        return p.ExprPlus + [p.Expr]

    # the base case for Expr+ (a single Expr)
    @_('Expr')
    def ExprPlus(self, p):
        print("Found ExprPlus with Expr")
        # Expr+ can now either be Expr or [Expr, Expr, ...]
        return [p.Expr]
    
    # Expr -> Ident = Expr
    @_('ID ASSIGN Expr')
    def Expr(self, p):
        print("Found Expr with ID = Expr")
        return (p.ID, p.ASSIGN, p.Expr)
    
    # Expr -> Ident
    @_('ID')
    def Expr(self, p):
        print("Found Expr with ID")
        return p.ID
    
    # Expr -> Constant
    @_('Constant')
    def Expr(self, p):
        print("Found Expr with Constant")
        return p.Constant
    
    # Expr -> Call
    @_('Call')
    def Expr(self, p):
        print("Found Expr with Call")
        return p.Call
    
    # Expr -> ( Expr )
    @_('LPAREN Expr RPAREN')
    def Expr(self, p):
        print("Found Expr with ( Expr )")
        return (p.LPAREN, p.Expr, p.RPAREN)
    
    # Expr -> Expr + Expr
    @_('Expr PLUS Expr')
    def Expr(self, p):
        print("Found Expr with Expr + Expr")
        return (p.Expr0, p.PLUS, p.Expr1)
    
    # Expr -> Expr - Expr
    @_('Expr MINUS Expr')
    def Expr(self, p):
        print("Found Expr with Expr - Expr")
        return (p.Expr0, p.MINUS, p.Expr1)
    
    # Expr -> Expr * Expr
    @_('Expr TIMES Expr')
    def Expr(self, p):
        print("Found Expr with Expr * Expr")
        return (p.Expr0, p.TIMES, p.Expr1)
    
    # Expr -> Expr / Expr
    @_('Expr DIVIDE Expr')
    def Expr(self, p):
        print("Found Expr with Expr / Expr")
        return (p.Expr0, p.DIVIDE, p.Expr1)
    
    # Expr -> Expr % Expr
    @_('Expr MODULUS Expr')
    def Expr(self, p):
        print("Found Expr with Expr mod Expr")
        return (p.Expr0, p.MODULUS, p.Expr1)
    
    # Expr -> - Expr
    @_('MINUS Expr %prec UMINUS')
    def Expr(self, p):
        print("Found Expr with - Expr")
        return (p.MINUS, p.Expr)
    
    # Expr -> Expr < Expr
    @_('Expr LT Expr')
    def Expr(self, p):
        print("Found Expr with Expr < Expr")
        return (p.Expr0, p.LT, p.Expr1)
    
    # Expr -> Expr <= Expr
    @_('Expr LE Expr')
    def Expr(self, p):
        print("Found Expr with Expr <= Expr")
        return (p.Expr0, p.LE, p.Expr1)
    
    # Expr -> Expr > Expr
    @_('Expr GT Expr')
    def Expr(self, p):
        print("Found Expr with Expr > Expr")
        return (p.Expr0, p.GT, p.Expr1)
    
    # Expr -> Expr >= Expr
    @_('Expr GE Expr')
    def Expr(self, p):
        print("Found Expr with Expr >= Expr")
        return (p.Expr0, p.GE, p.Expr1)
    
    # Expr -> Expr == Expr
    @_('Expr EQ Expr')
    def Expr(self, p):
        print("Found Expr with Expr == Expr")
        return (p.Expr0, p.EQ, p.Expr1)
    
    # Expr -> Expr != Expr
    @_('Expr NE Expr')
    def Expr(self, p):
        print("Found Expr with Expr != Expr")
        return (p.Expr0, p.NE, p.Expr1)
    
    # Expr -> Expr && Expr
    @_('Expr AND Expr')
    def Expr(self, p):
        print("Found Expr with Expr && Expr")
        return (p.Expr0, p.AND, p.Expr1)
    
    # Expr -> Expr || Expr
    @_('Expr OR Expr')
    def Expr(self, p):
        print("Found Expr with Expr || Expr")
        return (p.Expr0, p.OR, p.Expr1)
    
    # Expr -> ! Expr
    @_('NOT Expr')
    def Expr(self, p):
        print("Found Expr with ! Expr")
        return (p.NOT, p.Expr)
    
    # Expr -> InputInt ()
    @_('INPUTINT LPAREN RPAREN')
    def Expr(self, p):
        print("Found Expr with InputInt ()")
        return (p.INPUTINT, p.LPAREN, p.RPAREN)
    
    # Expr -> InputLine ()
    @_('INPUTLINE LPAREN RPAREN')
    def Expr(self, p):
        print("Found Expr with InputLine ()")
        return (p.INPUTLINE, p.LPAREN, p.RPAREN)
    
    # Call -> ident (Actuals)
    @_('ID LPAREN Actuals RPAREN')
    def Call(self, p):
        print("Found Call")
        return (p.ID, p.LPAREN, p.Actuals, p.RPAREN)

    # Actuals -> Expr+ , 
    @_('ExprPlus')
    def Actuals(self, p):
        print("Found Actuals with ExprList")
        return p.ExprPlus

    # Actuals -> empty
    @_('')
    def Actuals(self, p):
        print("Found empty Actuals")
        return []

    # Constant -> intConstant
    @_('INTCONST')
    def Constant(self, p):
        print("Found Constant with INTCONST")
        return p.INTCONST

    # Constant -> doubleConstant
    @_('DOUBLECONST')
    def Constant(self, p):
        print("Found Constant with DOUBLECONST")
        return p.DOUBLECONST

    # Constant -> boolConstant
    @_('TRUE')
    def Constant(self, p):
        print("Found Constant with TRUE")
        return p.TRUE
    
    # Constant -> boolConstant
    @_('FALSE')
    def Constant(self, p):
        print("Found Constant with FALSE")
        return p.FALSE

    # Constant -> stringConstant
    @_('STRINGCONST')
    def Constant(self, p):
        print("Found Constant with STRINGCONST")
        return p.STRINGCONST

    # Constant -> null
    @_('NULL')
    def Constant(self, p):
        print("Found Constant with NULL")
        return p.NULL
    
    # handling syntax errors
    def error(self, p):
        # if p exists (we're not at the end of the file)
        if p:
            # print the syntax error's token value and line number
            print(f"Sly: Syntax error at line {p.lineno} token = {p.value}")
        else:
            print("Sly: Syntax error at EOF")
        # attempt error recovery (keep going after error)
        self.errok()
    
if __name__ == '__main__':
    # store the test file name
    filename = 'test-parser.dlang'
    
    # open and read the file 
    # with statement automatically closes the file
    with open(filename, 'r') as file:
        data = file.read()
        
    # create a lexer
    lexer = DLangLexer()
    # initialize the symbol table
    lexer.initialize_symbol_table()
    # create a parser
    parser = DLangParser()
    
    # parse the file after tokenizing the contents
    result = parser.parse(lexer.tokenize(data))
    
    # show the result
    print(result)