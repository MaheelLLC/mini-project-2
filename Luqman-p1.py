from sly import Lexer
import re

class CalcLexer(Lexer):
    
    # String containing ignored characters between tokens
    ignore = ' \t'
    # we need to ignore carriage returns
    ignore_carriage_return = r'\r'
    # ignore single line comments (negate the single line comment if a new line
    # starts as well) Any character after // is ignored if it isn't a new line
    ignore_comment = r'//[^\n]*'
    # ignore multi line comments. [\s\S] matches to any whitespace and
    # non-whitespace character (in reality, this is just all characters in 
    # between /* */. The ? looks for the first time */ appears to find exactly
    # where the comment closes
    ignore_multiline_comment = r'/\*[\s\S]*?\*/'
    # ignore new lines
    ignore_newline = r'\n+'
    
    # let's store all of the keywords for DLang
    keywords = {'nothing', 'int', 'double', 'bool', 'string', 'class', 
                'interface', 'null', 'this', 'extends', 'implements', 'for', 
                'while', 'if', 'else', 'return', 'break', 'new', 
                'ArrayInstance', 'Output', 'InputInt', 'InputLine'}
    
    # store the two booleans
    booleans = {'True', 'False'}
    
    # store identifiers, integers, doubles, operators, and strings (since they 
    # are all regular expressions by value (these tokens have names that 
    # are not the values "capitalized" like the last two groups)
    original_tokens = { IDENTIFIER, INTEGER, DOUBLE, STRING, PLUS, MINUS, 
                       TIMES, DIVIDE, MODULUS, LESS, LESSANDEQUAL, GREATER, 
                       GREATERANDEQUAL, ASSIGN, EQUAL, NOTEQUAL, AND, OR, NOT, 
                       SEMICOLON, COMMA, DOT, LBRACKET, RBRACKET, LPARENTHESIS, 
                       RPARENTHESIS, LCURLYBRACE, RCURLYBRACE }
    
    # store the keyword token names as uppercase versions of keywords
    keyword_tokens = {keyword.upper() for keyword in keywords}
    # store the boolean token names as uppercase versions of booleans
    boolean_tokens = {boolean.upper() for boolean in booleans}
    # all of our tokens is the union of the three prior token groups
    tokens = original_tokens | keyword_tokens | boolean_tokens
    
    # Regular expression rules for tokens
    # this regular expression was given by the documentation for IDs
    IDENTIFIER = r'[a-zA-Z_][a-zA-Z0-9_]*'
    # double is a number followed by a dot, (possibly) followed by another
    # number, and (possibly) followed by E, (possibly) a sign, and another 
    # number
    DOUBLE = r'[0-9]+\.[0-9]*(E[+-]?[0-9]+)?'
    # An integer is just a number with only digits
    INTEGER = r'[0-9]+'
    # a string is any group of characters enclosed by double quotes and doesn't
    # include a newline or double quote inside of it
    STRING = r'"[^"\n]*"'
    # next 4 are given by documentation demo
    PLUS    = r'\+'
    MINUS   = r'-'
    TIMES   = r'\*'
    DIVIDE  = r'/'
    # the modulus operator in DLang
    MODULUS = r'%'
    # I placed lessandequal and greaterandequal above their counterparts so
    # the lexer recognizes them first (I did the same for double over integer,
    # notequal over not, and equal over assign). I basiclally established 
    # precedence in searching for longer tokens over shorter ones.
    # operators for DLang
    LESSANDEQUAL = r'<='
    LESS = r'<'
    GREATERANDEQUAL = r'>='
    GREATER = r'>'
    EQUAL = r'=='
    NOTEQUAL = r'!='
    ASSIGN  = r'='
    # more operators for DLang
    AND = r'&&'
    # forward slash used since | is a regular expression character
    OR = r'\|\|'
    NOT = r'!'
    # punctuation characters for DLang
    SEMICOLON = r';'
    COMMA = r','
    # forward slashes for the same reason as above
    DOT = r'\.'
    LBRACKET = r'\['
    RBRACKET = r'\]'
    LPARENTHESIS  = r'\('
    RPARENTHESIS  = r'\)'
    LCURLYBRACE = r'{'
    RCURLYBRACE = r'}'

    # connect this function with the token IDENTIFIER (every time the lexical 
    # analyzer encounters an IDENTIFIER token, this function will run)
    @_(IDENTIFIER)
    def IDENTIFIER(self, token):
        # if the token's value is one of DLang's keywords
        if token.value in self.keywords:
            # convert its token name to be the keyword in uppercase
            token.type = token.value.upper()   
        # if the token's value is one of DLang's booleans
        elif token.value in self.booleans:
            # convert its token name to be the boolean in uppercase
            token.type = token.value.upper()
        # (we get here if it's actually an IDENTIFIER token) and if the
        # IDENTIFIER token is longer than 50 characters
        elif len(token.value) > 50:
            # run the error handler for the token
            self.error(token)
            # don't return the bad token
            return None
        # return the (potentially changed) token
        return token
    
    # everytime the user provides an invalid input, this function will run
    def error(self, token):
        # if the user didn't close their quote
        if token.value[0] == '"':
            # notify them and provide a nice suggestion
            print(f'Missing end quote ("). I suggest placing the quote here: {token.value}"')
        # if their token is an IDENTIFIER and is longer than 50 characters
        elif token.type == "IDENTIFIER" and len(token.value) > 50:
            # notify them and tell them to come up with a shorter name
            print(f"{token.value} is longer than 50 characters. I suggest coming up with a shorter name such as {token.value[0:50]}.")
        # we have hit a token that doesn't exist in Dlang
        else:
            # let's remember where the first bad token is located
            start_index = self.index
            
            # when we hit any of these characters, we have found a good token
            # that we can start again from
            # we'll compile all valid characters in DLang into a single 
            # regular expression
            valid_start = re.compile(
                r'[a-zA-Z0-9"\+\-\*/%<>=!&\|\.;,\.\[\]\(\)\{\}\s]')
            
            # while we haven't reached the end of the input data
            while self.index < len(self.text):
                # if we found a good character (one that exists in DLang)
                if valid_start.match(self.text[self.index]):
                    # get out of this loop to continue lexical analysis
                    break
                # else, we are hitting another bad character
                else:
                    # move on to the next character
                    self.index += 1
                    
            # all characters between start_index and the current index are bad
            bad_chunk = self.text[start_index : self.index]
            # let's show them to the user and suggest deleting them
            print(f"Invalid input chunk: '{bad_chunk}'. I would suggest deleting them.")
            
            # we don't need to increment self.index again (done below)
            return None
                    
        # move on to the next character to continue analysis
        self.index += 1
        
if __name__ == '__main__':
    # initialize the lexer
    lexer = CalcLexer()
    # while the user still wants to input data
    while True:
        # request input from the user
        user_data = input("Please provide input for lexical analysis (enter q to quit program): ")
        # if the user desires to quit
        if user_data == "q":
            # break out of the program loop
            break
        # tokenize the user's data
        for tok in lexer.tokenize(user_data):
            # print out each token and its value
            print('type=%r, value=%r' % (tok.type, tok.value))